;;; codex-cli.el --- Codex CLI integration  -*- lexical-binding: t; -*-
;; Author: Benn <bennmsg@gmail.com>
;; Maintainer: Benn <bennmsg@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, codex, codex-cli
;; URL: https://github.com/bennfocus/codex-cli.el

;; This file is not part of GNU Emacs.

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
      (codex-cli--setup-side-window buffer))))

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
    ;; Show in side window
    (codex-cli--setup-side-window buffer)))

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
  (let ((buffer (codex-cli--get-or-create-buffer)))
    ;; Kill process if alive
    (when (codex-cli--alive-p buffer)
      (codex-cli--kill-process buffer))
    ;; Bury the buffer (default behavior - could be made configurable)
    (bury-buffer buffer)))

;;;###autoload
(defun codex-cli-send-prompt ()
  "Read a multi-line prompt and send it to the codex CLI."
  (interactive)
  (let ((buffer (codex-cli--get-or-create-buffer)))
    (unless (codex-cli--alive-p buffer)
      (error "Codex CLI process not running. Use `codex-cli-start' first"))
    
    (let ((prompt (read-string "Prompt: " nil nil nil t)))
      (when (and prompt (> (length prompt) 0))
        (codex-cli--log-and-send buffer prompt "prompt")))))

;;;###autoload
(defun codex-cli-copy-last-block ()
  "Re-send the last injected block verbatim."
  (interactive)
  (let ((buffer (codex-cli--get-or-create-buffer))
        (last-block (codex-cli--get-last-block)))
    (unless (codex-cli--alive-p buffer)
      (error "Codex CLI process not running. Use `codex-cli-start' first"))
    
    (if (and last-block (> (length last-block) 0))
        (progn
          (codex-cli--log-injection (codex-cli--project-name) "resend" last-block)
          (codex-cli--chunked-send buffer last-block codex-cli-max-bytes-per-send))
      (message "No previous block to resend"))))

;;;###autoload
(defun codex-cli-send-region ()
  "Send active region or whole buffer as fenced code block with language tag."
  (interactive)
  (let ((buffer (codex-cli--get-or-create-buffer)))
    (unless (codex-cli--alive-p buffer)
      (error "Codex CLI process not running. Use `codex-cli-start' first"))
    
    (let* ((start (if (region-active-p) (region-beginning) (point-min)))
           (end (if (region-active-p) (region-end) (point-max)))
           (content (buffer-substring-no-properties start end))
           (language (codex-cli--detect-language))
           (filepath (when buffer-file-name
                       (codex-cli-relpath buffer-file-name)))
           (fenced-block (codex-cli--format-fenced-block content language filepath)))
      
      (when (and (not (region-active-p))
                 (not (y-or-n-p "No active region. Send whole buffer? ")))
        (user-error "Cancelled"))
      
      (codex-cli--log-and-send buffer fenced-block "region"))))

;;;###autoload
(defun codex-cli-send-file ()
  "Prompt for file under project and send as fenced block with chunking."
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
      
      (let* ((content (with-temp-buffer
                        (insert-file-contents file-path)
                        (buffer-string)))
             (relpath (codex-cli-relpath file-path))
             (ext (file-name-extension file-path))
             (language (codex-cli--detect-language-from-extension ext))
             (fenced-block (codex-cli--format-fenced-block content language relpath)))
        
        (message "Sending %s..." relpath)
        (codex-cli--log-and-send buffer fenced-block "file")))))

(provide 'codex-cli)

;;; codex-cli.el ends here
