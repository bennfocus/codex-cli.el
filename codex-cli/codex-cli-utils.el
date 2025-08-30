;;; codex-cli-utils.el --- Utility functions for codex-cli -*- lexical-binding: t; -*-
;; Author: Benn <bennmsg@gmail.com>
;; Maintainer: Benn <bennmsg@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools convenience codex codex-cli
;; URL: https://github.com/bennfocus/codex-cli.el

;;; Commentary:
;; Fence formatting, language guessing, echo-area progress, and ring for last
;; injected block for codex-cli.

;;; Code:

(defvar codex-cli--last-block ""
  "The last block that was sent to the CLI.")

(defun codex-cli--store-last-block (text)
  "Store TEXT as the last block sent."
  (setq codex-cli--last-block text))

(defun codex-cli--get-last-block ()
  "Return the last block that was sent."
  codex-cli--last-block)

(defvar codex-cli--mode-language-map
  '((emacs-lisp-mode . "elisp")
    (lisp-interaction-mode . "elisp")
    (python-mode . "python")
    (js-mode . "javascript")
    (js2-mode . "javascript")
    (typescript-mode . "typescript")
    (json-mode . "json")
    (yaml-mode . "yaml")
    (html-mode . "html")
    (css-mode . "css")
    (sh-mode . "bash")
    (shell-script-mode . "bash")
    (elixir-mode . "elixir")
    (go-mode . "go")
    (rust-mode . "rust")
    (php-mode . "php")
    (java-mode . "java")
    (c-mode . "c")
    (c++-mode . "cpp")
    (sql-mode . "sql"))
  "Map of major modes to language tags for fenced code blocks.")

(defvar codex-cli--extension-language-map
  '(("el" . "elisp")
    ("py" . "python")
    ("js" . "javascript")
    ("ts" . "typescript")
    ("json" . "json")
    ("yaml" . "yaml")
    ("yml" . "yaml")
    ("html" . "html")
    ("css" . "css")
    ("sh" . "bash")
    ("bash" . "bash")
    ("ex" . "elixir")
    ("exs" . "elixir")
    ("go" . "go")
    ("rs" . "rust")
    ("php" . "php")
    ("java" . "java")
    ("c" . "c")
    ("cpp" . "cpp")
    ("cc" . "cpp")
    ("sql" . "sql"))
  "Map of file extensions to language tags for fenced code blocks.")

(defun codex-cli--detect-language ()
  "Detect the language tag for the current buffer.
Returns nil if language cannot be determined."
  (or (cdr (assoc major-mode codex-cli--mode-language-map))
      (when buffer-file-name
        (let ((ext (file-name-extension buffer-file-name)))
          (when ext
            (cdr (assoc ext codex-cli--extension-language-map)))))))

(defun codex-cli--format-fenced-block (content &optional language filepath)
  "Format CONTENT as a fenced code block with optional LANGUAGE tag.
If FILEPATH is provided, include a header with the file path."
  (let ((lang-tag (or language ""))
        (header (when filepath (format "# File: %s\n" filepath))))
    (concat header
            "```" lang-tag "\n"
            content
            (unless (string-suffix-p "\n" content) "\n")
            "```")))

(defun codex-cli--detect-language-from-extension (extension)
  "Detect language tag from file EXTENSION."
  (when extension
    (cdr (assoc extension codex-cli--extension-language-map))))

(defun codex-cli--log-buffer-name (project-name)
  "Return the log buffer name for PROJECT-NAME."
  (format "*codex-cli-log:%s*" project-name))

(defun codex-cli--log-injection (project-name operation text)
  "Log injection to the log buffer if enabled.
PROJECT-NAME is the project name, OPERATION is the type of operation,
and TEXT is the injected content."
  (when (bound-and-true-p codex-cli-log-injections)
    (let* ((log-buffer-name (codex-cli--log-buffer-name project-name))
           (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
           (log-entry (format "[%s] %s:\n%s\n\n" timestamp operation text)))
      (with-current-buffer (get-buffer-create log-buffer-name)
        (goto-char (point-max))
        (insert log-entry)))))

(provide 'codex-cli-utils)
;;; codex-cli-utils.el ends here
