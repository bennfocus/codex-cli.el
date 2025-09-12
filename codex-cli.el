;;; codex-cli.el --- Codex CLI integration  -*- lexical-binding: t; -*-
;; Author: Benn <bennmsg@gmail.com>
;; Maintainer: Benn <bennmsg@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools convenience codex codex-cli
;; URL: https://github.com/bennfocus/codex-cli.el
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Run Codex CLI in an Emacs terminal buffer per project and provide minimal
;; helpers to send context: region, file, arbitrary text. Predictable window
;; management with a small surface area.

;;; Code:

(require 'codex-cli-project)
(require 'codex-cli-term)
(require 'codex-cli-utils)
(require 'seq)
(require 'subr-x)

(declare-function codex-cli--alive-p "codex-cli-term")
(declare-function codex-cli--kill-process "codex-cli-term")
(declare-function codex-cli--start-terminal-process "codex-cli-term")
(declare-function codex-cli--chunked-send "codex-cli-term")
(declare-function codex-cli--chunked-send-raw "codex-cli-term")
(declare-function codex-cli--send-return "codex-cli-term")
(declare-function codex-cli--store-last-block "codex-cli-utils")
(declare-function codex-cli--get-last-block "codex-cli-utils")
(declare-function codex-cli--detect-language "codex-cli-utils")
(declare-function codex-cli--format-fenced-block "codex-cli-utils")
(declare-function codex-cli--detect-language-from-extension "codex-cli-utils")
(declare-function codex-cli--log-injection "codex-cli-utils")

(defgroup codex-cli nil
  "Run Codex CLI inside Emacs with minimal helpers."
  :group 'tools
  :prefix "codex-cli-")

(defcustom codex-cli-executable "codex"
  "Path to the Codex CLI binary."
  :type 'string
  :group 'codex-cli)

(defcustom codex-cli-extra-args nil
  "List of extra args passed to Codex CLI."
  :type '(repeat string)
  :group 'codex-cli)

(defcustom codex-cli-side 'right
  "Side window placement: left or right."
  :type '(choice (const left) (const right))
  :group 'codex-cli)

(defcustom codex-cli-width 90
  "Side window width in columns."
  :type 'integer
  :group 'codex-cli)

(defcustom codex-cli-terminal-backend 'vterm
  "Preferred terminal backend: vterm or term."
  :type '(choice (const vterm) (const term))
  :group 'codex-cli)

(defcustom codex-cli-max-bytes-per-send 8000
  "Chunk size when sending large content."
  :type 'integer
  :group 'codex-cli)

(defcustom codex-cli-session-preamble nil
  "Optional text to inject once after CLI starts."
  :type '(choice (const nil) string)
  :group 'codex-cli)

(defcustom codex-cli-log-injections t
  "If non-nil, mirror injected blocks into a log buffer."
  :type 'boolean
  :group 'codex-cli)

;; Window focus behavior
(defcustom codex-cli-focus-on-open t
  "When non-nil, select the Codex side window after displaying it.
Affects `codex-cli-start' and send commands like
`codex-cli-send-region', `codex-cli-send-file', and
`codex-cli-copy-last-block'. Note: `codex-cli-send-prompt' never
changes focus and always keeps point in the current window."
  :type 'boolean
  :group 'codex-cli)

;; Sending style and reference formatting
(defcustom codex-cli-send-style 'fenced
  "How to send region/file content to Codex CLI.
When `fenced', paste full content wrapped in a fenced code block.
When `reference', send a file reference token like `@path#L10-20' instead."
  :type '(choice (const fenced) (const reference))
  :group 'codex-cli)

(defcustom codex-cli-reference-prefix ""
  "Optional prefix inserted before reference tokens.
For example, set to \"i \" to send `i @file#L10-20'."
  :type 'string
  :group 'codex-cli)

(defcustom codex-cli-reference-format-single "@%s#L%d"
  "Format string for a single-line file reference.
`%s' is the relative path, `%d' is the line number."
  :type 'string
  :group 'codex-cli)

(defcustom codex-cli-reference-format-range "@%s#L%d-%d"
  "Format string for a line-range file reference.
The `%s' is the relative path; the first `%d' is the start line and
the second `%d' is the end line."
  :type 'string
  :group 'codex-cli)

(defcustom codex-cli-reference-file-format "@%s"
  "Format string for a whole-file reference.
`%s' is the relative path."
  :type 'string
  :group 'codex-cli)

(defun codex-cli--format-reference-for-region (relpath start-line end-line)
  "Return a reference token for RELPATH covering START-LINE..END-LINE.
Respects `codex-cli-reference-prefix' and formatting defcustoms."
  (concat (if codex-cli-reference-prefix (format "%s" codex-cli-reference-prefix) "")
          (if (= start-line end-line)
              (format codex-cli-reference-format-single relpath start-line)
            (format codex-cli-reference-format-range relpath start-line end-line))))

(defun codex-cli--format-reference-for-file (relpath)
  "Return a whole-file reference token for RELPATH."
  (concat (if codex-cli-reference-prefix (format "%s" codex-cli-reference-prefix) "")
          (format codex-cli-reference-file-format relpath)))

(defun codex-cli--project-name ()
  "Return a short name for the current project."
  (let ((root (codex-cli-project-root)))
    (file-name-nondirectory (directory-file-name root))))

;; Session tracking per project root
(defvar codex-cli--last-session-by-project (make-hash-table :test 'equal)
  "Hashtable mapping project roots to last-used session names.
Empty string means the default session.")

(defun codex-cli--generate-session-id ()
  "Generate a short random session id string (lowercase hex)."
  (let* ((n1 (random most-positive-fixnum))
         (n2 (random most-positive-fixnum))
         (pid (if (fboundp 'emacs-pid) (emacs-pid) 0))
         (mix (logxor n1 n2 pid))
         (id (format "%x" mix)))
    ;; Normalize to a compact 6–8 char token
    (substring id 0 (min 8 (length id)))))

(defun codex-cli--project-root-key ()
  "Return the key used for per-project hash maps (the project root path)."
  (codex-cli-project-root))

(defun codex-cli--buffer-name (&optional session)
  "Return the Codex buffer name for the current project and optional SESSION.
When SESSION is nil or empty, returns the default session name."
  (let ((proj (codex-cli--project-name)))
    (if (and session (> (length session) 0))
        (format "*codex-cli:%s:%s*" proj session)
      (format "*codex-cli:%s*" proj))))

(defun codex-cli--parse-buffer-name (buffer-or-name)
  "Parse BUFFER-OR-NAME and return (PROJECT SESSION) if it is a Codex buffer.
SESSION may be nil when default. Returns nil if not a Codex buffer."
  (let* ((name (if (bufferp buffer-or-name) (buffer-name buffer-or-name) buffer-or-name)))
    (when (string-match "^\\*codex-cli:\\([^:*]+\\)\(?::\\([^*]+\\)\)?\\*$" name)
      (list (match-string 1 name)
            (let ((sess (match-string 2 name)))
              (and sess (string-trim sess)))))))

(defun codex-cli--sessions-for-project ()
  "Return a list of session name strings for the current project.
The default session is represented as an empty string \"\"."
  (condition-case _err
      (let* ((proj (codex-cli--project-name))
             (prefix (format "*codex-cli:%s" proj))
             (sessions '())
             (seen-default nil))
        (dolist (buf (buffer-list))
          (let* ((name (buffer-name buf)))
            (when (string-prefix-p prefix name)
              (let ((parts (codex-cli--parse-buffer-name name)))
                (when parts
                  (let ((sess (cadr parts)))
                    (if sess
                        (push sess sessions)
                      (setq seen-default t))))))))
        (when seen-default (push "" sessions))
        (delete-dups (nreverse sessions)))
    (error nil)))

;; Global session helpers (cross-project)
(defun codex-cli--all-session-buffers ()
  "Return a list of all Codex session buffers across all projects."
  (seq-filter (lambda (b)
                (let ((name (buffer-name b)))
                  (and name
                       (string-prefix-p "*codex-cli:" name)
                       (not (string-prefix-p "*codex-cli-log:" name)))))
              (buffer-list)))

(defun codex-cli--visible-session-buffer ()
  "Return a visible Codex session buffer across any project, if one exists."
  (seq-find (lambda (b) (get-buffer-window b))
            (codex-cli--all-session-buffers)))

(defun codex-cli--choose-any-session-buffer (&optional prompt)
  "Prompt user to choose a Codex session buffer across any project.
Return the chosen buffer or nil when none exist."
  (let* ((buffers (codex-cli--all-session-buffers)))
    (cond
     ((null buffers) nil)
     ((= (length buffers) 1) (car buffers))
     (t
      (let* ((candidates (mapcar (lambda (b)
                                    (let* ((parts (codex-cli--parse-buffer-name b))
                                           (proj (car parts))
                                           (sess (cadr parts))
                                           (label (if sess
                                                      (format "%s:%s" proj sess)
                                                    (format "%s:default" proj))))
                                      (cons label b)))
                                  buffers))
             (choice (completing-read (or prompt "Choose session (any project): ")
                                      (mapcar #'car candidates) nil t)))
        (cdr (assoc choice candidates)))))))

(defun codex-cli--record-last-session (session)
  "Record SESSION as the last-used session for this project. Empty means default."
  (puthash (codex-cli--project-root-key) (or session "") codex-cli--last-session-by-project))

(defun codex-cli--last-session ()
  "Return last-used session name for the project, or empty string if none."
  (or (gethash (codex-cli--project-root-key) codex-cli--last-session-by-project) ""))

(defun codex-cli--get-or-create-buffer (&optional session)
  "Get or create the codex buffer for the current project SESSION.
SESSION is a string; nil/empty selects the default session."
  (let ((buffer-name (codex-cli--buffer-name session)))
    (or (get-buffer buffer-name)
        (get-buffer-create buffer-name))))

(defun codex-cli--focus-buffer (&optional session)
  "Focus the codex buffer for the current project and SESSION.
If the buffer exists, switch to it. Otherwise, create it first."
  (let ((buffer (codex-cli--get-or-create-buffer session)))
    (switch-to-buffer buffer)
    buffer))

(defun codex-cli--setup-side-window (buffer)
  "Display BUFFER in a side window according to configuration."
  (display-buffer-in-side-window
   buffer
   `((side . ,codex-cli-side)
     (window-width . ,codex-cli-width))))

(defun codex-cli--show-and-maybe-focus (buffer)
  "Ensure BUFFER is shown in a side window and maybe focus it.
Returns the window displaying BUFFER. When
`codex-cli-focus-on-open' is non-nil, selects that window."
  (let ((win (or (get-buffer-window buffer)
                 (codex-cli--setup-side-window buffer))))
    (when (and codex-cli-focus-on-open (window-live-p win))
      (select-window win))
    win))

(defun codex-cli--side-window-visible-p (buffer)
  "Return t if BUFFER is currently visible in any window."
  (and (buffer-live-p buffer) (get-buffer-window buffer)))

(defun codex-cli--visible-buffer-for-project ()
  "Return a visible codex buffer for the current project, if any."
  (let* ((proj (codex-cli--project-name))
         (prefix (format "*codex-cli:%s" proj)))
    (seq-find (lambda (buf)
                (and (string-prefix-p prefix (buffer-name buf))
                     (get-buffer-window buf)))
              (buffer-list))))

(defun codex-cli--resolve-target-buffer (&optional session create)
  "Resolve a codex buffer for the project and SESSION.
If CREATE is non-nil, create the buffer when absent. Otherwise return
nil when missing. When SESSION is nil, prefer visible buffer, then
last-used, then the default session."
  (let* ((sess (or session
                   (when-let ((buf (codex-cli--visible-buffer-for-project)))
                     (cadr (codex-cli--parse-buffer-name buf)))
                   (codex-cli--last-session)))
         (name (codex-cli--buffer-name sess))
         (buf (get-buffer name)))
    (cond
     (buf buf)
     (create (codex-cli--get-or-create-buffer sess))
     (t nil))))

;; Internal helper to choose an existing session interactively, preferring
;; auto-pick when only one exists. Returns the chosen session string or nil.
(defun codex-cli--choose-existing-session (&optional prompt)
  "Return a session from existing project sessions, or nil if none.
If exactly one session exists, return it without prompting.
Otherwise prompt with PROMPT using completion."
  (let* ((sessions (codex-cli--sessions-for-project)))
    (cond
     ((null sessions)
      ;; No sessions in this project — caller decides on cross-project fallback
      nil)
     ((= (length sessions) 1)
      (car sessions))
     (t
      (let* ((display-sessions (mapcar (lambda (s) (if (string-empty-p s) "default" s)) sessions))
             (choice (completing-read (or prompt "Choose session: ") display-sessions nil t)))
        (if (string= choice "default") "" choice))))))

;;;###autoload
(defun codex-cli-toggle (&optional session)
  "Toggle the side window for SESSION without killing the process.
When called interactively without SESSION, choose from existing sessions
with completion; if only one session exists, select it automatically."
  (interactive)
  (let* ((sess (or session (codex-cli--choose-existing-session "Toggle session: ")))
         (buffer (and sess (codex-cli--resolve-target-buffer sess t))))
    (cond
     (buffer
      (codex-cli--record-last-session (codex-cli--session-name-for-buffer buffer))
      (if (codex-cli--side-window-visible-p buffer)
          (when-let ((window (get-buffer-window buffer))) (delete-window window))
        (codex-cli--show-and-maybe-focus buffer)))
     (t
      ;; Final fallback: allow toggling any Codex session across projects
      (if-let ((any-buf (codex-cli--choose-any-session-buffer "Toggle session (any project): ")))
          (if (codex-cli--side-window-visible-p any-buf)
              (when-let ((window (get-buffer-window any-buf))) (delete-window window))
            (codex-cli--show-and-maybe-focus any-buf))
        (message "No Codex sessions found"))))))

;;;###autoload
(defun codex-cli-start-or-toggle (&optional session)
  "Start Codex CLI for SESSION if not running; otherwise toggle its side window.
If SESSION is nil, use visible/last-used/default session."
  (interactive
   (list (when current-prefix-arg
           (read-string "Session (empty = default): " nil nil ""))))
  (let ((buffer (codex-cli--resolve-target-buffer session nil)))
    (if (and buffer (codex-cli--alive-p buffer))
        (codex-cli-toggle (codex-cli--session-name-for-buffer buffer))
      (codex-cli-start session))))

(defvar-local codex-cli--preamble-timer nil
  "Timer for preamble injection after process start (buffer-local).")

(defun codex-cli--session-name-for-buffer (buffer)
  "Return session name string for BUFFER, or empty string for default."
  (let* ((parts (codex-cli--parse-buffer-name buffer)))
    (or (cadr parts) "")))

(defun codex-cli--log-and-store (buffer text operation)
  "Log TEXT with OPERATION for BUFFER and store last-block in that BUFFER."
  (let ((project-name (codex-cli--project-name))
        (session (codex-cli--session-name-for-buffer buffer)))
    (codex-cli--log-injection project-name operation text session)
    (with-current-buffer buffer
      (codex-cli--store-last-block text))))

(defun codex-cli--log-and-send (buffer text operation)
  "Log TEXT with OPERATION type and send to BUFFER."
  (codex-cli--log-and-store buffer text operation)
  (codex-cli--chunked-send buffer text codex-cli-max-bytes-per-send))

(defun codex-cli--inject-preamble (buffer)
  "Inject session preamble into BUFFER if configured."
  (when (and codex-cli-session-preamble
             (codex-cli--alive-p buffer))
    (codex-cli--log-and-send buffer codex-cli-session-preamble "preamble")))

;;;###autoload
(defun codex-cli-start (&optional session)
  "Start a NEW Codex CLI session in the current project.
If SESSION is nil or empty, generate a random session id.
This command always creates a new session; it never reuses an existing one."
  (interactive
   (list (when current-prefix-arg
           (read-string "New session name (blank = auto): " nil nil ""))))
  (let* ((project-root (codex-cli-project-root))
         (desired (and (stringp session) (string-trim session)))
         ;; Auto-generate if empty or not provided
         (name (if (and desired (> (length desired) 0)) desired (codex-cli--generate-session-id)))
         ;; Ensure uniqueness when auto-generating; if user provided a duplicate, error
         (existing (codex-cli--sessions-for-project)))
    (when (member name existing)
      (if (or (null desired) (string-empty-p desired))
          (while (member name existing)
            (setq name (codex-cli--generate-session-id)))
        (user-error "Session '%s' already exists. Choose a different name" name)))
    (let* ((buffer (codex-cli--get-or-create-buffer name)))
      (codex-cli--start-terminal-process
       buffer
       project-root
       codex-cli-executable
       codex-cli-extra-args
       codex-cli-terminal-backend)
      ;; Schedule preamble injection after a short delay
      (when codex-cli-session-preamble
        (with-current-buffer buffer
          (when codex-cli--preamble-timer
            (cancel-timer codex-cli--preamble-timer))
          (setq codex-cli--preamble-timer
                (run-with-timer 1.0 nil #'codex-cli--inject-preamble buffer))))
      ;; Show in side window and maybe focus
      (codex-cli--record-last-session (codex-cli--session-name-for-buffer buffer))
      (codex-cli--show-and-maybe-focus buffer))))

;; Update restart to use the new start logic
;;;###autoload
(defun codex-cli-restart (&optional session)
  "Kill existing process and start a new one in the same SESSION buffer."
  (interactive
   (list (when current-prefix-arg
           (read-string "Restart session (empty = default): " nil nil ""))))
  (let ((buffer (codex-cli--get-or-create-buffer session)))
    (when (codex-cli--alive-p buffer)
      (codex-cli--kill-process buffer))
    ;; Start a new process
    (codex-cli-start session)))

;;;###autoload
(defun codex-cli-stop (&optional session)
  "Kill the process and bury the buffer for SESSION.
When called interactively without SESSION, choose from existing sessions
with completion; if only one session exists, select it automatically."
  (interactive)
  (let* ((sess (or session (codex-cli--choose-existing-session "Stop session: ")))
         (buffer (and sess (get-buffer (codex-cli--buffer-name sess)))))
    (when buffer
      (codex-cli--record-last-session (codex-cli--session-name-for-buffer buffer))
      ;; Close any window(s) displaying the buffer first
      (dolist (win (get-buffer-window-list buffer nil t))
        (when (window-live-p win)
          (delete-window win)))
      ;; Kill process if alive
      (when (codex-cli--alive-p buffer)
        (codex-cli--kill-process buffer))
      ;; Only bury if buffer is still live
      (when (buffer-live-p buffer)
        (bury-buffer buffer)))))

;;;###autoload
(defun codex-cli-send-prompt (&optional session)
  "Read a multi-line prompt and send it to Codex in SESSION."
  (interactive
   (list (when current-prefix-arg
           (read-string "Send prompt to session (empty = default): " nil nil ""))))
  (let ((buffer (codex-cli--resolve-target-buffer session nil)))
    (unless (and buffer (codex-cli--alive-p buffer))
      (error "Codex CLI process not running. Use `codex-cli-start' first"))

    (let ((prompt (read-string "Prompt: " nil nil nil t)))
      (when (and prompt (> (length prompt) 0))
        ;; Ensure the window exists but do not move focus
        (unless (get-buffer-window buffer)
          (codex-cli--setup-side-window buffer))
        ;; Log + store, then send without trailing newline and press Enter
        (codex-cli--log-and-store buffer prompt "prompt")
        (codex-cli--chunked-send-raw buffer prompt codex-cli-max-bytes-per-send)
        (codex-cli--send-return buffer)))))

;;;###autoload
(defun codex-cli-copy-last-block (&optional session)
  "Re-send the last injected block for SESSION verbatim."
  (interactive
   (list (when current-prefix-arg
           (read-string "Resend from session (empty = default): " nil nil ""))))
  (let* ((buffer (codex-cli--resolve-target-buffer session nil)))
    (unless (and buffer (codex-cli--alive-p buffer))
      (error "Codex CLI process not running. Use `codex-cli-start' first"))
    (with-current-buffer buffer
      (let ((last-block (codex-cli--get-last-block)))
        ;; Ensure the window is visible and focused per user preference
        (codex-cli--show-and-maybe-focus buffer)
        (if (and last-block (> (length last-block) 0))
            (progn
              (codex-cli--log-and-store buffer last-block "resend")
              (codex-cli--chunked-send buffer last-block codex-cli-max-bytes-per-send))
          (message "No previous block to resend"))))))

;;;###autoload
(defun codex-cli-send-region (&optional session)
  "Send active region or whole buffer to Codex CLI.
Behavior depends on `codex-cli-send-style':
- `fenced': send content as a fenced code block with language tag.
- `reference': send a file reference token like `@path#Lstart-end' if the
  buffer is visiting a file; otherwise fallback to `fenced'."
  (interactive
   (list (when current-prefix-arg
           (read-string "Send region to session (empty = default): " nil nil ""))))
  (let ((buffer (codex-cli--resolve-target-buffer session nil)))
    (unless (and buffer (codex-cli--alive-p buffer))
      (error "Codex CLI process not running. Use `codex-cli-start' first"))

    (let* ((start (if (region-active-p) (region-beginning) (point-min)))
           (end (if (region-active-p) (region-end) (point-max))))

      (when (and (not (region-active-p))
                 (not (y-or-n-p "No active region. Send whole buffer? ")))
        (user-error "Cancelled"))

      (cond
       ((eq codex-cli-send-style 'reference)
        (if (not buffer-file-name)
            ;; No file path; fallback to fenced content
            (let* ((content (buffer-substring-no-properties start end))
                   (language (codex-cli--detect-language))
                   (fenced (codex-cli--format-fenced-block content language nil)))
              (codex-cli--show-and-maybe-focus buffer)
              (codex-cli--log-and-send buffer fenced "region"))
          (let* ((relpath (codex-cli-relpath buffer-file-name))
                 (start-line (save-excursion (goto-char start) (line-number-at-pos)))
                 (end-line (save-excursion
                             (goto-char (if (> end (point-min)) (1- end) end))
                             (line-number-at-pos)))
                 (ref (codex-cli--format-reference-for-region relpath start-line end-line)))
            (codex-cli--show-and-maybe-focus buffer)
            (codex-cli--log-and-send buffer ref "region-ref"))))
       (t
        ;; fenced (default)
        (let* ((content (buffer-substring-no-properties start end))
               (language (codex-cli--detect-language))
               (filepath (when buffer-file-name
                           (codex-cli-relpath buffer-file-name)))
               (fenced-block (codex-cli--format-fenced-block content language filepath)))
          (codex-cli--show-and-maybe-focus buffer)
          (codex-cli--log-and-send buffer fenced-block "region")))))))

;;;###autoload
(defun codex-cli-send-file (&optional session)
  "Prompt for file under project and send according to `codex-cli-send-style'.
When `fenced', send file content as a fenced block with chunking.
When `reference', send an `@path' token instead of content."
  (interactive
   (list (when current-prefix-arg
           (read-string "Send file to session (empty = default): " nil nil ""))))
  (let ((buffer (codex-cli--resolve-target-buffer session nil)))
    (unless (and buffer (codex-cli--alive-p buffer))
      (error "Codex CLI process not running. Use `codex-cli-start' first"))

    (let* ((project-root (codex-cli-project-root))
           (file-path (read-file-name "Send file: " project-root nil t)))

      ;; Check if file is inside project root
      (unless (string-prefix-p project-root (expand-file-name file-path))
        (error "File must be inside project root: %s" project-root))

      (unless (file-readable-p file-path)
        (error "File not readable: %s" file-path))

      (let* ((relpath (codex-cli-relpath file-path)))
        (cond
         ((eq codex-cli-send-style 'reference)
          (let ((ref (codex-cli--format-reference-for-file relpath)))
            (message "Sending reference %s" ref)
            (codex-cli--show-and-maybe-focus buffer)
            (codex-cli--log-and-send buffer ref "file-ref")))
         (t
          (let* ((content (with-temp-buffer
                             (insert-file-contents file-path)
                             (buffer-string)))
                 (ext (file-name-extension file-path))
                 (language (codex-cli--detect-language-from-extension ext))
                 (fenced (codex-cli--format-fenced-block content language relpath)))
            (message "Sending %s..." relpath)
            (codex-cli--show-and-maybe-focus buffer)
            (codex-cli--log-and-send buffer fenced "file"))))))))

;;; Session management helpers/commands

(defun codex-cli--read-session-name (&optional prompt allow-empty)
  "Prompt for a session name with completion from existing sessions.
PROMPT overrides the default prompt. When ALLOW-EMPTY is non-nil,
an empty string selects the default session."
  (let* ((sessions (codex-cli--sessions-for-project))
         (display-sessions (mapcar (lambda (s) (if (string-empty-p s) "default" s)) sessions))
         (input (completing-read (or prompt "Session (default = empty): ")
                                 display-sessions nil nil nil nil
                                 (when allow-empty "default"))))
    (if (and allow-empty (string= input "default"))
        ""
      input)))

;;;###autoload
(defun codex-cli-start-session (name)
  "Start a new Codex session NAME in the current project."
  (interactive (list (read-string "New session name (blank = auto): " nil nil "")))
  (codex-cli-start name))

;;;###autoload
(defun codex-cli-toggle-session (name)
  "Toggle Codex session NAME in the current project."
  (interactive (list (codex-cli--choose-existing-session "Toggle session: ")))
  (when name (codex-cli-toggle name)))

;;;###autoload
(defun codex-cli-stop-session (name)
  "Stop Codex session NAME in the current project."
  (interactive (list (codex-cli--choose-existing-session "Stop session: ")))
  (when name (codex-cli-stop name)))

;;;###autoload
(defun codex-cli-list-sessions ()
  "List existing Codex sessions for the current project."
  (interactive)
  (let* ((sessions (codex-cli--sessions-for-project))
         (formatted (mapconcat (lambda (s) (if (string-empty-p s) "default" s))
                               (sort (copy-sequence sessions) #'string-lessp)
                               ", ")))
    (message (if (string-empty-p formatted)
                 "No sessions yet"
               (format "Sessions: %s" formatted)))))

;;;###autoload
(defun codex-cli-stop-all ()
  "Stop all Codex sessions for the current project."
  (interactive)
  (let* ((proj (codex-cli--project-name))
         (prefix (format "*codex-cli:%s" proj))
         (targets (seq-filter (lambda (b) (string-prefix-p prefix (buffer-name b)))
                              (buffer-list))))
    (dolist (buffer targets)
      ;; Close windows
      (dolist (win (get-buffer-window-list buffer nil t))
        (when (window-live-p win) (delete-window win)))
      ;; Kill process
      (when (codex-cli--alive-p buffer)
        (codex-cli--kill-process buffer))
      ;; Bury
      (when (buffer-live-p buffer)
        (bury-buffer buffer)))))

(provide 'codex-cli)
;;; codex-cli.el ends here
