;;; codex-cli.el --- Codex CLI integration  -*- lexical-binding: t; -*-
;; Author: Benn <bennmsg@gmail.com>
;; Maintainer: Benn <bennmsg@gmail.com>
;; Version: 0.1.3
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
(require 'cl-lib)

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

;; Forward var declaration to silence byte-compiler when referenced earlier
(defvar codex-cli--preamble-timer)

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

(defcustom codex-cli-toggle-all-min-width 50
  "Minimum column width per session when showing all sessions.
Used by `codex-cli-toggle-all' to decide how many session columns
to display at once. When the current frame cannot accommodate all
sessions at this width, sessions are split across pages."
  :type 'integer
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
`codex-cli-send-region' and `codex-cli-send-file'. Note: `codex-cli-send-prompt' never
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
  "Return a unique identifier for the current project based on its path.
The path is abbreviated (e.g., `~` for home), has any trailing slash
removed, and any `:` characters are replaced with the Unicode ratio
colon `∶` to avoid conflicts with our buffer name separators."
  (let* ((root (codex-cli-project-root))
         (abbr (abbreviate-file-name root))
         (no-trailing (directory-file-name abbr)))
    (replace-regexp-in-string ":" "∶" no-trailing)))

(defun codex-cli--project-name-from-root (root)
  "Return project display name string derived from ROOT path.
Performs the same transformation as `codex-cli--project-name', but for
an explicit ROOT instead of the current project."
  (let* ((abbr (abbreviate-file-name root))
         (no-trailing (directory-file-name abbr)))
    (replace-regexp-in-string ":" "∶" no-trailing)))

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
    (cond
     ;; Named session: *codex-cli:PROJECT:SESSION*
     ((string-match "^\\*codex-cli:\\([^:*]+\\):\\([^*]+\\)\\*$" name)
      (list (match-string 1 name)
            (string-trim (match-string 2 name))))
     ;; Default session: *codex-cli:PROJECT*
     ((string-match "^\\*codex-cli:\\([^:*]+\\)\\*$" name)
      (list (match-string 1 name) nil))
     (t nil))))

(defun codex-cli--sessions-for-project ()
  "Return a list of session name strings for the current project.
The default session is represented as an empty string \"\".
Implementation parses Codex buffer names and filters by current project name."
  (condition-case err
      (let* ((proj (codex-cli--project-name))
             (sessions '())
             (seen-default nil))
        (dolist (buf (codex-cli--all-session-buffers))
          (let ((parts (codex-cli--parse-buffer-name buf)))
            (when parts
              (let ((proj-name (car parts))
                    (sess (cadr parts)))
                (when (string= proj proj-name)
                  (if (and sess (> (length sess) 0))
                      (push sess sessions)
                    (setq seen-default t)))))))
        (when seen-default (push "" sessions))
        (delete-dups (nreverse sessions)))
    (error
     (message "codex-cli--sessions-for-project error: %s (buffer=%s dir=%s)"
              (error-message-string err) (buffer-name) default-directory)
     nil)))

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

;; Project-scoped session buffer helpers
(defun codex-cli--project-session-buffers ()
  "Return a list of Codex session buffers for the current project path."
  (let* ((proj (codex-cli--project-name))
         (prefix (format "*codex-cli:%s" proj)))
    (seq-filter (lambda (b)
                  (let ((n (buffer-name b)))
                    (and n (string-prefix-p prefix n) (not (string-prefix-p "*codex-cli-log:" n)))))
                (buffer-list))))

(defun codex-cli--project-session-buffers-for-root (root)
  "Return a list of Codex session buffers for the project ROOT path."
  (let* ((proj (codex-cli--project-name-from-root root))
         (prefix (format "*codex-cli:%s" proj)))
    (seq-filter (lambda (b)
                  (let ((n (buffer-name b)))
                    (and n (string-prefix-p prefix n)
                         (not (string-prefix-p "*codex-cli-log:" n)))))
                (buffer-list))))

(defun codex-cli--choose-project-session-buffer (&optional prompt)
  "Prompt to choose a Codex session buffer within the current project.
Returns the chosen buffer or nil when none exist. Shows the project path
and session name during selection, for example ~/proj/path:abc123."
  (let* ((buffers (codex-cli--project-session-buffers)))
    (cond
     ((null buffers) nil)
     ((= (length buffers) 1) (car buffers))
     (t
      (let* ((candidates (mapcar (lambda (b)
                                   (let* ((parts (codex-cli--parse-buffer-name b))
                                          (proj (car parts))
                                          (sess (cadr parts))
                                          (label (if sess (format "%s:%s" proj sess) proj)))
                                     (cons label b)))
                                 buffers))
             (choice (completing-read (or prompt "Choose session: ")
                                      (mapcar #'car candidates) nil t)))
        (cdr (assoc choice candidates))))))
  )

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

;; State for `codex-cli-toggle-all'
(defvar codex-cli--toggle-all-config-by-frame (make-hash-table :test 'eq)
  "Saved window configurations by frame for `codex-cli-toggle-all'.")

(defvar codex-cli--toggle-all-state-by-frame (make-hash-table :test 'eq)
  "State by frame for `codex-cli-toggle-all'. Each value is a plist with
keys :project-root, :page (zero-based integer).")

(defun codex-cli--toggle-all-active-p ()
  "Return non-nil if `codex-cli-toggle-all' layout is active in this frame."
  (gethash (selected-frame) codex-cli--toggle-all-config-by-frame))

(defun codex-cli--toggle-all--windows-left-to-right ()
  "Return windows in the selected frame sorted left-to-right."
  (let ((wins (window-list (selected-frame) 'no-mini)))
    (sort wins (lambda (a b)
                 (< (car (window-edges a))
                    (car (window-edges b)))))))

(defun codex-cli--toggle-all--per-page (total)
  "Compute how many sessions to show per page given TOTAL sessions."
  (let* ((root (frame-root-window))
         (width (max 1 (window-total-width root)))
         (minw (max 1 codex-cli-toggle-all-min-width))
         (max-columns (max 1 (/ width minw))))
    (min total max-columns)))

(defun codex-cli--toggle-all--show-page (page buffers)
  "Show PAGE (0-based) of BUFFERS as vertical columns across the frame."
  (let* ((total (length buffers))
         (per (codex-cli--toggle-all--per-page total))
         (pages (max 1 (ceiling (/ (float total) (float per)))))
         (page (max 0 (min page (1- pages))))
         (start (* page per))
         (end (min total (+ start per)))
         (slice (cl-subseq buffers start end)))
    ;; Ensure we operate from a non-side window to avoid errors like
    ;; "Cannot make side window the only window" when calling
    ;; `delete-other-windows'. Prefer the frame's main window when
    ;; available, otherwise pick any window without a `window-side'
    ;; parameter.
    (let* ((main (and (fboundp 'window-main-window)
                      (window-main-window (selected-frame))))
           (non-side (or (and (window-live-p main) main)
                         (seq-find (lambda (w)
                                     (not (window-parameter w 'window-side)))
                                   (window-list (selected-frame) 'no-mini)))))
      (when (window-live-p non-side)
        (select-window non-side)))
    (delete-other-windows)
    ;; Create N-1 vertical splits, then balance
    (when (> (length slice) 1)
      (dotimes (_ (1- (length slice)))
        (split-window-right))
      (balance-windows))
    ;; Map buffers to windows left->right
    (let ((wins (codex-cli--toggle-all--windows-left-to-right)))
      (cl-mapc #'set-window-buffer wins slice))
    ;; Return normalized page and page count
    (list page pages per)))

;;;###autoload
(defun codex-cli-toggle-all ()
  "Toggle showing all project sessions as columns in the current frame.

First call saves the current window configuration and arranges all
sessions from this project into evenly sized vertical columns. Each
column has at least `codex-cli-toggle-all-min-width' columns. When the
frame is too narrow to show all sessions at this minimum width, the
sessions are split across pages. Use `codex-cli-toggle-all-next-page'
and `codex-cli-toggle-all-prev-page' to navigate between pages.

Calling the command again in the same frame restores the previous
window configuration."
  (interactive)
  (let ((frame (selected-frame)))
    (if (codex-cli--toggle-all-active-p)
        ;; Restore previous layout
        (let ((conf (gethash frame codex-cli--toggle-all-config-by-frame)))
          (when conf (set-window-configuration conf))
          (remhash frame codex-cli--toggle-all-config-by-frame)
          (remhash frame codex-cli--toggle-all-state-by-frame)
          (message "codex-cli: restored previous window layout"))
      ;; Activate multi-session layout
      (let* ((orig-conf (current-window-configuration))
             (buffers (codex-cli--project-session-buffers)))
        ;; If no sessions for this project, offer to create one.
        (when (null buffers)
          (when (y-or-n-p "No session in this project. Start a new one? ")
            ;; Start a new session WITHOUT displaying a side window
            (let* ((project-root (codex-cli-project-root))
                   (name (codex-cli--generate-session-id))
                   (buffer (codex-cli--get-or-create-buffer name)))
              (codex-cli--start-terminal-process
               buffer project-root codex-cli-executable codex-cli-extra-args codex-cli-terminal-backend)
              (when codex-cli-session-preamble
                (with-current-buffer buffer
                  (when codex-cli--preamble-timer
                    (cancel-timer codex-cli--preamble-timer))
                  (setq codex-cli--preamble-timer
                        (run-with-timer 1.0 nil #'codex-cli--inject-preamble buffer))))
              (codex-cli--record-last-session (codex-cli--session-name-for-buffer buffer))
              (setq buffers (list buffer)))))
        (if (null buffers)
            (message "codex-cli: no sessions to display")
          (progn
            ;; Save original window configuration (pre-creation, pre-layout)
            (puthash frame orig-conf codex-cli--toggle-all-config-by-frame)
            ;; Always use the column layout + paging (works for 1+ sessions)
            (cl-destructuring-bind (page pages per)
                (codex-cli--toggle-all--show-page 0 buffers)
              (puthash frame (list :project-root (codex-cli-project-root)
                                   :page page
                                   :pages pages
                                   :per-page per)
                       codex-cli--toggle-all-state-by-frame)
              (message "codex-cli: showing %d/%d sessions (page %d/%d)"
                       (min per (length buffers)) (length buffers) (1+ page) pages))))))))

;;;###autoload
(defun codex-cli-toggle-all-next-page ()
  "Show the next page of sessions for `codex-cli-toggle-all' in this frame."
  (interactive)
  (let* ((frame (selected-frame))
         (state (gethash frame codex-cli--toggle-all-state-by-frame)))
    (unless state
      (user-error "codex-cli-toggle-all is not active in this frame"))
    (let* ((buffers (codex-cli--project-session-buffers-for-root (plist-get state :project-root)))
           (per (codex-cli--toggle-all--per-page (length buffers)))
           (pages (max 1 (ceiling (/ (float (length buffers)) (float per)))))
           (curr (plist-get state :page))
           (page (if (>= curr (1- pages)) 0 (1+ curr))))
      (cl-destructuring-bind (page* pages* per*)
          (codex-cli--toggle-all--show-page page buffers)
        (puthash frame (list :project-root (codex-cli-project-root)
                             :page page*
                             :pages pages*
                             :per-page per*)
                 codex-cli--toggle-all-state-by-frame)
        (message "codex-cli: page %d/%d" (1+ page*) pages*)))))

;;;###autoload
(defun codex-cli-toggle-all-prev-page ()
  "Show the previous page of sessions for `codex-cli-toggle-all' in this frame."
  (interactive)
  (let* ((frame (selected-frame))
         (state (gethash frame codex-cli--toggle-all-state-by-frame)))
    (unless state
      (user-error "codex-cli-toggle-all is not active in this frame"))
    (let* ((buffers (codex-cli--project-session-buffers-for-root (plist-get state :project-root)))
           (per (codex-cli--toggle-all--per-page (length buffers)))
           (pages (max 1 (ceiling (/ (float (length buffers)) (float per)))))
           (curr (plist-get state :page))
           (page (if (<= curr 0) (1- pages) (1- curr))))
      (cl-destructuring-bind (page* pages* per*)
          (codex-cli--toggle-all--show-page page buffers)
        (puthash frame (list :project-root (codex-cli-project-root)
                             :page page*
                             :pages pages*
                             :per-page per*)
                 codex-cli--toggle-all-state-by-frame)
        (message "codex-cli: page %d/%d" (1+ page*) pages*)))))

;;;###autoload
(defun codex-cli-toggle (&optional session)
  "Toggle the side window for SESSION within the current project.
Behavior:
- If no session exists in this project, offer to create a new one.
- If one session exists, toggle it.
- If multiple exist, always prompt to choose a session. The chooser shows
  the full buffer name including the project path. If SESSION is provided,
  toggle that session explicitly.

When called from a Codex session buffer, switching between sessions is
done in-place within the current window (no side-window recreation), so
window size is preserved. Toggling the same session from within its
buffer simply hides the window."
  (interactive)
  (let* ((buffers (codex-cli--project-session-buffers))
         (current-codex (codex-cli--parse-buffer-name (current-buffer)))
         (current-window (and current-codex (get-buffer-window (current-buffer)))))
    (if (null buffers)
        ;; No sessions detected for this project. Offer to create.
        (when (y-or-n-p "No session in this project. Start a new one? ")
          (codex-cli-start))
      (let* ((target
              (cond
               ;; Explicit session argument takes precedence
               ((and session (stringp session) (> (length (string-trim session)) 0))
                (get-buffer (codex-cli--buffer-name (string-trim session))))
               ;; One existing buffer: use it directly
               ((= (length buffers) 1)
                (car buffers))
               ;; Multiple: always prompt, showing full buffer name
               (t (codex-cli--choose-project-session-buffer "Toggle session: ")))))
        (cond
         ((not (buffer-live-p target))
          (when (and session (stringp session))
            (message "Session '%s' not found in this project" session)))
         (current-window
          ;; In a Codex buffer: operate in-place to preserve window size
          (codex-cli--record-last-session (codex-cli--session-name-for-buffer target))
          (if (eq (window-buffer current-window) target)
              (delete-window current-window)
            (set-window-buffer current-window target)))
         (t
          ;; Not in a Codex buffer: normal toggle/show behavior
          (codex-cli--record-last-session (codex-cli--session-name-for-buffer target))
          (if (codex-cli--side-window-visible-p target)
              (when-let ((window (get-buffer-window target))) (delete-window window))
            (codex-cli--show-and-maybe-focus target)))))))
  )

;; codex-cli-start-or-toggle removed: prefer `codex-cli-toggle` which will
;; offer to create a new session when none exist for the project.

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
  "Restart Codex in a chosen session buffer for the current project.
When multiple sessions exist, prompts using the same path:session labels
as `codex-cli-toggle`. If SESSION is provided, restarts that session."
  (interactive)
  (let* ((buffer
          (cond
           ((and session (stringp session) (> (length (string-trim session)) 0))
            (get-buffer (codex-cli--buffer-name (string-trim session))))
           (t
            (let ((bufs (codex-cli--project-session-buffers)))
              (cond
               ((null bufs) nil)
               ((= (length bufs) 1) (car bufs))
               (t (codex-cli--choose-project-session-buffer "Restart session: "))))))))
    (unless (buffer-live-p buffer)
      (user-error "No session to restart. Use `codex-cli-start` first"))
    (let ((sess (codex-cli--session-name-for-buffer buffer)))
      (when (codex-cli--alive-p buffer)
        (codex-cli--kill-process buffer))
      ;; Start a new process in the same session buffer
      (codex-cli-start sess))))

;;;###autoload
(defun codex-cli-stop (&optional session)
  "Kill the process and bury the buffer for SESSION.
When called interactively without SESSION, choose from existing sessions
using the same path:session chooser as `codex-cli-toggle`. If only one
session exists, select it automatically."
  (interactive)
  (let* ((buffer
          (cond
           ((and session (stringp session) (> (length (string-trim session)) 0))
            (get-buffer (codex-cli--buffer-name (string-trim session))))
           (t
            (let ((bufs (codex-cli--project-session-buffers)))
              (cond
               ((null bufs) nil)
               ((= (length bufs) 1) (car bufs))
               (t (codex-cli--choose-project-session-buffer "Stop session: "))))))))
    (if (not (buffer-live-p buffer))
        (when (and session (stringp session))
          (message "Session '%s' not found in this project" session))
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
  "Read a multi-line prompt and send it to Codex in a chosen session.
Uses the same path:session chooser as `codex-cli-toggle` when multiple
sessions exist. If SESSION is provided, sends to that session."
  (interactive)
  (let* ((buffer
          (cond
           ((and session (stringp session) (> (length (string-trim session)) 0))
            (get-buffer (codex-cli--buffer-name (string-trim session))))
           (t
            (let ((bufs (codex-cli--project-session-buffers)))
              (cond
               ((null bufs) nil)
               ((= (length bufs) 1) (car bufs))
               (t (codex-cli--choose-project-session-buffer "Send prompt to: "))))))))
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
(defun codex-cli-send-region (&optional session)
  "Send active region or whole buffer to a chosen Codex session.
Uses the same path:session chooser as `codex-cli-toggle` when multiple
sessions exist. Behavior depends on `codex-cli-send-style':
- `fenced': send content as a fenced code block with language tag.
- `reference': send a file reference token like `@path#Lstart-end' if the
  buffer is visiting a file; otherwise fallback to `fenced'."
  (interactive)
  (let* ((buffer
          (cond
           ((and session (stringp session) (> (length (string-trim session)) 0))
            (get-buffer (codex-cli--buffer-name (string-trim session))))
           (t
            (let ((bufs (codex-cli--project-session-buffers)))
              (cond
               ((null bufs) nil)
               ((= (length bufs) 1) (car bufs))
               (t (codex-cli--choose-project-session-buffer "Send region to: "))))))))
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
  (interactive)
  (let* ((buffer
          (cond
           ((and session (stringp session) (> (length (string-trim session)) 0))
            (get-buffer (codex-cli--buffer-name (string-trim session))))
           (t
            (let ((bufs (codex-cli--project-session-buffers)))
              (cond
               ((null bufs) nil)
               ((= (length bufs) 1) (car bufs))
               (t (codex-cli--choose-project-session-buffer "Send file to: "))))))))
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

(defun codex-cli--validate-session-name (name)
  "Validate session NAME. Must be non-empty and avoid reserved characters.
Signals a user error if invalid. Returns NAME otherwise."
  (when (or (not (stringp name)) (string-empty-p name))
    (user-error "Session name cannot be empty"))
  (when (string-match-p "[:*]" name)
    (user-error "Session name cannot contain ':' or '*'"))
  name)

;;;###autoload
(defun codex-cli-rename-session (&optional old-session new-session)
  "Rename a Codex session within the current project.

When called interactively without arguments, prompt to choose an
existing session (showing project path and session), then prompt for
the NEW-SESSION name. Enter an empty name to make it the default
session (no explicit id in the buffer name).

If OLD-SESSION and NEW-SESSION are provided non-interactively, rename
that session directly. If called while current-buffer is a Codex
session buffer, that buffer is renamed without prompting to choose.
Signals an error if the target name collides. The new session name is
required and cannot be empty."
  (interactive)
  (let* ((proj (codex-cli--project-name))
         (current-name (buffer-name (current-buffer)))
         (current-session-buffer
          (and current-name
               (string-prefix-p "*codex-cli:" current-name)
               (not (string-prefix-p "*codex-cli-log:" current-name))
               (current-buffer)))
         (buffer
          (or current-session-buffer
              (and old-session (get-buffer (codex-cli--buffer-name old-session)))
              (codex-cli--choose-project-session-buffer "Rename session (choose): "))))
    (unless (buffer-live-p buffer)
      (user-error "No session selected"))
    (let* ((parts (codex-cli--parse-buffer-name buffer))
           (current-session (or (cadr parts) ""))
           (desired (or new-session
                        (read-string (format "New session name for %s: " proj)
                                     nil nil nil))))
      (codex-cli--validate-session-name desired)
      (let* ((target-name (codex-cli--buffer-name desired)))
        (when (get-buffer target-name)
          (user-error "Target session already exists: %s" target-name))
        ;; Rename session buffer
        (with-current-buffer buffer
          (rename-buffer target-name t))
        ;; Update last-session mapping when renaming the last one
        (when (string= (codex-cli--last-session) current-session)
          (codex-cli--record-last-session desired))
        ;; Also rename the log buffer when present
        (let* ((old-log (get-buffer (codex-cli--log-buffer-name proj current-session)))
               (new-log-name (codex-cli--log-buffer-name proj desired)))
          (when (buffer-live-p old-log)
            (with-current-buffer old-log
              (rename-buffer new-log-name t))))
        (message "Renamed session '%s' -> '%s'" current-session desired)))))

;; codex-cli-start-session removed: use `codex-cli-start` directly.

;; codex-cli-toggle-session removed: use `codex-cli-toggle` directly.

;; codex-cli-stop-session removed: use `codex-cli-stop` directly with chooser.

;; codex-cli-list-sessions removed: rely on chooser prompts in commands.

;;;###autoload
(defun codex-cli-stop-all (&optional scope)
  "Stop Codex sessions in bulk.

Interactively prompts to choose between stopping sessions for the
current project or for all projects. When SCOPE is provided
non-interactively, it should be the symbol `project' or `all'."
  (interactive)
  (let* ((scope-choice
          (cond
           ((memq scope '(project all)) scope)
           (t (let* ((input (completing-read
                             "Stop sessions for: "
                             '("current project" "all projects")
                             nil t nil nil "current project")))
                (if (string= input "all projects") 'all 'project)))))
         (targets (if (eq scope-choice 'all)
                      (codex-cli--all-session-buffers)
                    (codex-cli--project-session-buffers))))
    (if (null targets)
        (message "No Codex sessions to stop")
      (dolist (buffer targets)
        ;; Close windows
        (dolist (win (get-buffer-window-list buffer nil t))
          (when (window-live-p win) (delete-window win)))
        ;; Kill process
        (when (codex-cli--alive-p buffer)
          (codex-cli--kill-process buffer))
        ;; Bury
        (when (buffer-live-p buffer)
          (bury-buffer buffer))))))

(provide 'codex-cli)
;;; codex-cli.el ends here
