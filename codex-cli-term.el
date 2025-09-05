;;; codex-cli-term.el --- Terminal abstraction for codex-cli -*- lexical-binding: t; -*-
;; Author: Benn <bennmsg@gmail.com>
;; Maintainer: Benn <bennmsg@gmail.com>
;; SPDX-License-Identifier: MIT
;; Keywords: tools convenience codex codex-cli
;; URL: https://github.com/bennfocus/codex-cli.el

;;; Commentary:
;; Terminal abstraction for codex-cli. Start vterm or term, send strings with
;; proper escapes, chunking, and liveness checks.

;;; Code:

(declare-function vterm-mode "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function term-mode "term")
(declare-function term-exec "term")
(declare-function term-send-raw-string "term")

(defvar codex-cli--vterm-fallback-warned nil
  "Non-nil if we've already warned about vterm fallback to term.")

(defun codex-cli--vterm-available-p ()
  "Return non-nil if vterm is available to load.
Tries to require it lazily; returns nil if not installed."
  (or (featurep 'vterm)
      (require 'vterm nil t)))

(defun codex-cli--start-vterm-process (buffer project-root command args)
  "Start a vterm process in BUFFER at PROJECT-ROOT running COMMAND with ARGS."
  (require 'vterm)
  (with-current-buffer buffer
    (let ((default-directory project-root))
      (vterm-mode)
      (vterm-send-string (concat command " " (mapconcat #'shell-quote-argument args " ")))
      (vterm-send-return))))

(defun codex-cli--start-term-process (buffer project-root command args)
  "Start a term process in BUFFER at PROJECT-ROOT running COMMAND with ARGS."
  (with-current-buffer buffer
    (let ((default-directory project-root))
      ;; Ensure term is loaded before invoking term-mode functions
      (require 'term)
      (term-mode)
      (term-exec buffer (buffer-name buffer) command nil args))))

(defun codex-cli--executable-available-p (command)
  "Return non-nil if COMMAND is available in PATH."
  (and command
       (or (file-executable-p command)
           (executable-find command))))

(defun codex-cli--start-terminal-process (buffer project-root command args backend)
  "Start terminal process in BUFFER at PROJECT-ROOT.
COMMAND is the executable, ARGS is a list of arguments.
BACKEND should be \='vterm or \='term."
  ;; Check if executable is available
  (unless (codex-cli--executable-available-p command)
    (error "Codex CLI executable not found: %s\nPATH: %s\nSet `codex-cli-executable' to the correct path"
           command (getenv "PATH")))

  (cond
   ((and (eq backend 'vterm) (codex-cli--vterm-available-p))
    (codex-cli--start-vterm-process buffer project-root command args))
   ((eq backend 'vterm)
    ;; vterm requested but not available, fallback to term
    (unless codex-cli--vterm-fallback-warned
      (message "vterm not available; using built-in term instead")
      (setq codex-cli--vterm-fallback-warned t))
    (codex-cli--start-term-process buffer project-root command args))
   (t
    ;; term backend requested or fallback
    (codex-cli--start-term-process buffer project-root command args))))

(defun codex-cli--alive-p (buffer)
  "Return t if the process in BUFFER is alive."
  (when (buffer-live-p buffer)
    (let ((proc (get-buffer-process buffer)))
      (and proc (process-live-p proc)))))

(defun codex-cli--kill-process (buffer)
  "Kill the process in BUFFER if it exists."
  (when (buffer-live-p buffer)
    (let ((proc (get-buffer-process buffer)))
      (when proc
        (delete-process proc)))))

(defun codex-cli--send-string (buffer text)
  "Send TEXT to the terminal process in BUFFER."
  (when (and (buffer-live-p buffer) (codex-cli--alive-p buffer))
    (with-current-buffer buffer
      (cond
       ((derived-mode-p 'vterm-mode)
        (require 'vterm)
        (vterm-send-string text))
       ((derived-mode-p 'term-mode)
        (term-send-raw-string text))
       (t
        (error "Buffer is not in vterm or term mode: %s" major-mode))))))

(defun codex-cli--chunked-send (buffer text max-bytes-per-send)
  "Send TEXT to BUFFER in chunks of MAX-BYTES-PER-SEND with delays.
Appends final newline once after all chunks are sent."
  (let* ((text-length (length text))
         (start 0)
         (chunk-num 1)
         (total-chunks (ceiling (/ (float text-length) max-bytes-per-send))))

    (while (< start text-length)
      (let* ((end (min (+ start max-bytes-per-send) text-length))
             (chunk (substring text start end)))

        ;; Show progress for multiple chunks
        (when (> total-chunks 1)
          (message "Sending chunk [%d/%d]..." chunk-num total-chunks))

        (codex-cli--send-string buffer chunk)

        ;; Sleep between chunks (except for the last one)
        (when (< end text-length)
          (sleep-for 0.01))

        (setq start end
              chunk-num (1+ chunk-num))))

    ;; Send final newline
    (codex-cli--send-string buffer "\n")

    ;; Clear progress message
    (when (> total-chunks 1)
      (message "Sending complete."))))

(defun codex-cli--chunked-send-raw (buffer text max-bytes-per-send)
  "Send TEXT to BUFFER in chunks without appending a newline.
Uses the same chunking behavior as `codex-cli--chunked-send' but does
not send a trailing newline."
  (let* ((text-length (length text))
         (start 0)
         (chunk-num 1)
         (total-chunks (ceiling (/ (float text-length) max-bytes-per-send))))
    (while (< start text-length)
      (let* ((end (min (+ start max-bytes-per-send) text-length))
             (chunk (substring text start end)))
        (when (> total-chunks 1)
          (message "Sending chunk [%d/%d]..." chunk-num total-chunks))
        (codex-cli--send-string buffer chunk)
        (when (< end text-length)
          (sleep-for 0.01))
        (setq start end
              chunk-num (1+ chunk-num))))
    (when (> total-chunks 1)
      (message "Sending complete."))))

(defun codex-cli--send-return (buffer)
  "Simulate pressing Enter/Return in the terminal BUFFER."
  (when (and (buffer-live-p buffer) (codex-cli--alive-p buffer))
    (with-current-buffer buffer
      (cond
       ((derived-mode-p 'vterm-mode)
        (require 'vterm)
        (vterm-send-return))
       ((derived-mode-p 'term-mode)
        ;; Carriage return is the correct Enter for term
        (term-send-raw-string "\r"))
       (t
        (error "Buffer is not in vterm or term mode: %s" major-mode))))))

(provide 'codex-cli-term)
;;; codex-cli-term.el ends here
