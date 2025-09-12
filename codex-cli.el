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

(defun codex-cli--buffer-name ()
  "Return the buffer name for the current project."
  (format "*codex-cli:%s*" (codex-cli--project-name)))

(defun codex-cli--get-or-create-buffer ()
  "Get or create the codex-cli buffer for the current project.
Returns the buffer, creating it if it doesn't exist."
  (let ((buffer-name (codex-cli--buffer-name)))
    (or (get-buffer buffer-name)
        (get-buffer-create buffer-name))))

(defun codex-cli--focus-buffer ()
  "Focus the codex-cli buffer for the current project.
If the buffer exists, switch to it. Otherwise, create it first."
  (let ((buffer (codex-cli--get-or-create-buffer)))
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

(defun codex-cli--side-window-visible-p ()
  "Return t if a codex-cli side window is currently visible."
  (let ((buffer (get-buffer (codex-cli--buffer-name))))
    (and buffer (get-buffer-window buffer))))

;;;###autoload
(defun codex-cli-toggle ()
  "Toggle visibility of the codex-cli side window without killing the process."
  (interactive)
  (let ((buffer (codex-cli--get-or-create-buffer)))
    (if (codex-cli--side-window-visible-p)
        ;; Hide the window
        (let ((window (get-buffer-window buffer)))
          (when window
            (delete-window window)))
      ;; Show the window
      (codex-cli--show-and-maybe-focus buffer))))

;;;###autoload
(defun codex-cli-start-or-toggle ()
  "Start Codex CLI if not running; otherwise toggle the side window.
If the project buffer/process does not exist, behaves like
`codex-cli-start'. If it is already running, behaves like
`codex-cli-toggle'."
  (interactive)
  (let* ((buffer-name (codex-cli--buffer-name))
         (buffer (get-buffer buffer-name)))
    (if (and buffer (codex-cli--alive-p buffer))
        (codex-cli-toggle)
      (codex-cli-start))))

(defvar codex-cli--preamble-timer nil
  "Timer for preamble injection after process start.")

(defun codex-cli--log-and-send (buffer text operation)
  "Log TEXT with OPERATION type and send to BUFFER."
  (let ((project-name (codex-cli--project-name)))
    (codex-cli--log-injection project-name operation text)
    (codex-cli--store-last-block text)
    (codex-cli--chunked-send buffer text codex-cli-max-bytes-per-send)))

(defun codex-cli--inject-preamble (buffer)
  "Inject session preamble into BUFFER if configured."
  (when (and codex-cli-session-preamble
             (codex-cli--alive-p buffer))
    (codex-cli--log-and-send buffer codex-cli-session-preamble "preamble")))

;;;###autoload
(defun codex-cli-start ()
  "Start Codex CLI in the current project or focus the existing buffer."
  (interactive)
  (let* ((project-root (codex-cli-project-root))
         (buffer (codex-cli--get-or-create-buffer))
         (was-running (codex-cli--alive-p buffer)))
    ;; Start process if not already running
    (unless was-running
      (codex-cli--start-terminal-process
       buffer
       project-root
       codex-cli-executable
       codex-cli-extra-args
       codex-cli-terminal-backend)
      ;; Schedule preamble injection after a short delay
      (when codex-cli-session-preamble
        (when codex-cli--preamble-timer
          (cancel-timer codex-cli--preamble-timer))
        (setq codex-cli--preamble-timer
              (run-with-timer 1.0 nil #'codex-cli--inject-preamble buffer))))
    ;; Show in side window and maybe focus
    (codex-cli--show-and-maybe-focus buffer)))

;; Update restart to use the new start logic
;;;###autoload
(defun codex-cli-restart ()
  "Kill existing process and start a new one in the same buffer."
  (interactive)
  (let ((buffer (codex-cli--get-or-create-buffer)))
    (when (codex-cli--alive-p buffer)
      (codex-cli--kill-process buffer))
    ;; Start a new process
    (codex-cli-start)))

;;;###autoload
(defun codex-cli-stop ()
  "Kill the process and bury the buffer."
  (interactive)
  (let* ((buffer-name (codex-cli--buffer-name))
         (buffer (get-buffer buffer-name)))
    (when buffer
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
(defun codex-cli-send-prompt ()
  "Read a multi-line prompt and send it to the codex CLI."
  (interactive)
  (let ((buffer (codex-cli--get-or-create-buffer)))
    (unless (codex-cli--alive-p buffer)
      (error "Codex CLI process not running. Use `codex-cli-start' first"))

    (let ((prompt (read-string "Prompt: " nil nil nil t)))
      (when (and prompt (> (length prompt) 0))
        ;; Ensure the window exists but do not move focus
        (unless (get-buffer-window buffer)
          (codex-cli--setup-side-window buffer))
        ;; Log + store, then send without trailing newline and press Enter
        (codex-cli--log-injection (codex-cli--project-name) "prompt" prompt)
        (codex-cli--store-last-block prompt)
        (codex-cli--chunked-send-raw buffer prompt codex-cli-max-bytes-per-send)
        (codex-cli--send-return buffer)))))

;;;###autoload
(defun codex-cli-copy-last-block ()
  "Re-send the last injected block verbatim."
  (interactive)
  (let ((buffer (codex-cli--get-or-create-buffer))
        (last-block (codex-cli--get-last-block)))
    (unless (codex-cli--alive-p buffer)
      (error "Codex CLI process not running. Use `codex-cli-start' first"))

    ;; Ensure the window is visible and focused per user preference
    (codex-cli--show-and-maybe-focus buffer)

    (if (and last-block (> (length last-block) 0))
        (progn
          (codex-cli--log-injection (codex-cli--project-name) "resend" last-block)
          (codex-cli--chunked-send buffer last-block codex-cli-max-bytes-per-send))
      (message "No previous block to resend"))))

;;;###autoload
(defun codex-cli-send-region ()
  "Send active region or whole buffer to Codex CLI.
Behavior depends on `codex-cli-send-style':
- `fenced': send content as a fenced code block with language tag.
- `reference': send a file reference token like `@path#Lstart-end' if the
  buffer is visiting a file; otherwise fallback to `fenced'."
  (interactive)
  (let ((buffer (codex-cli--get-or-create-buffer)))
    (unless (codex-cli--alive-p buffer)
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
(defun codex-cli-send-file ()
  "Prompt for file under project and send according to `codex-cli-send-style'.
When `fenced', send file content as a fenced block with chunking.
When `reference', send an `@path' token instead of content."
  (interactive)
  (let ((buffer (codex-cli--get-or-create-buffer)))
    (unless (codex-cli--alive-p buffer)
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

(provide 'codex-cli)
;;; codex-cli.el ends here
