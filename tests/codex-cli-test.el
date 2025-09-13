;;; codex-cli-test.el --- Tests for codex-cli -*- lexical-binding: t; -*-
;; Author: Benn <bennmsg@gmail.com>
;; Maintainer: Benn <bennmsg@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, codex, codex-cli
;; URL: https://github.com/bennfocus/codex-cli.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for codex-cli utilities and core functionality.

;;; Code:

(require 'ert)
(require 'codex-cli-utils)
(require 'codex-cli)
(require 'cl-lib)

(ert-deftest codex-cli-test--dummy ()
  "Placeholder test to ensure ERT harness works."
  (should t))

;; Session id generation
(ert-deftest codex-cli-test--generate-session-id ()
  "Generated session ids are non-empty short hex strings."
  (let ((id (codex-cli--generate-session-id)))
    (should (stringp id))
    (should (> (length id) 0))
    (should (<= (length id) 8))
    (should (string-match-p "^[0-9a-f]+$" id))))

;; Language detection tests
(ert-deftest codex-cli-test--detect-language-from-extension ()
  "Test language detection from file extensions."
  (should (string= "elisp" (codex-cli--detect-language-from-extension "el")))
  (should (string= "python" (codex-cli--detect-language-from-extension "py")))
  (should (string= "javascript" (codex-cli--detect-language-from-extension "js")))
  (should (string= "typescript" (codex-cli--detect-language-from-extension "ts")))
  (should (string= "json" (codex-cli--detect-language-from-extension "json")))
  (should (string= "yaml" (codex-cli--detect-language-from-extension "yaml")))
  (should (string= "yaml" (codex-cli--detect-language-from-extension "yml")))
  (should (string= "html" (codex-cli--detect-language-from-extension "html")))
  (should (string= "css" (codex-cli--detect-language-from-extension "css")))
  (should (string= "bash" (codex-cli--detect-language-from-extension "sh")))
  (should (string= "bash" (codex-cli--detect-language-from-extension "bash")))
  (should (string= "elixir" (codex-cli--detect-language-from-extension "ex")))
  (should (string= "elixir" (codex-cli--detect-language-from-extension "exs")))
  (should (string= "go" (codex-cli--detect-language-from-extension "go")))
  (should (string= "rust" (codex-cli--detect-language-from-extension "rs")))
  (should (string= "php" (codex-cli--detect-language-from-extension "php")))
  (should (string= "java" (codex-cli--detect-language-from-extension "java")))
  (should (string= "c" (codex-cli--detect-language-from-extension "c")))
  (should (string= "cpp" (codex-cli--detect-language-from-extension "cpp")))
  (should (string= "cpp" (codex-cli--detect-language-from-extension "cc")))
  (should (string= "sql" (codex-cli--detect-language-from-extension "sql")))
  ;; Unknown extension should return nil
  (should (null (codex-cli--detect-language-from-extension "unknown")))
  (should (null (codex-cli--detect-language-from-extension nil))))

;; Fenced block formatting tests
(ert-deftest codex-cli-test--format-fenced-block ()
  "Test fenced code block formatting."
  ;; Basic block with language
  (should (string= "```python\nprint('hello')\n```"
                   (codex-cli--format-fenced-block "print('hello')" "python")))
  
  ;; Block with no language
  (should (string= "```\nprint('hello')\n```"
                   (codex-cli--format-fenced-block "print('hello')")))
  
  ;; Block with file path
  (should (string= "# File: test.py\n```python\nprint('hello')\n```"
                   (codex-cli--format-fenced-block "print('hello')" "python" "test.py")))
  
  ;; Block with trailing newline preserved
  (should (string= "```python\nprint('hello')\n```"
                   (codex-cli--format-fenced-block "print('hello')\n" "python")))
  
  ;; Block with multiple lines
  (should (string= "```python\nprint('hello')\nprint('world')\n```"
                   (codex-cli--format-fenced-block "print('hello')\nprint('world')" "python"))))

;; Last block ring tests
(ert-deftest codex-cli-test--last-block-ring ()
  "Test last block storage and retrieval."
  ;; Store and retrieve
  (codex-cli--store-last-block "test content")
  (should (string= "test content" (codex-cli--get-last-block)))
  
  ;; Overwrite previous block
  (codex-cli--store-last-block "new content")
  (should (string= "new content" (codex-cli--get-last-block)))
  
  ;; Empty content
  (codex-cli--store-last-block "")
  (should (string= "" (codex-cli--get-last-block))))

;; Last block should be buffer-local (per-session)
(ert-deftest codex-cli-test--last-block-buffer-local ()
  "Ensure last block is buffer-local across buffers."
  (let ((b1 (generate-new-buffer " test-codex-1"))
        (b2 (generate-new-buffer " test-codex-2")))
    (unwind-protect
        (progn
          (with-current-buffer b1
            (codex-cli--store-last-block "one"))
          (with-current-buffer b2
            (codex-cli--store-last-block "two"))
          (with-current-buffer b1
            (should (string= "one" (codex-cli--get-last-block))))
          (with-current-buffer b2
            (should (string= "two" (codex-cli--get-last-block)))))
      (kill-buffer b1)
      (kill-buffer b2))))

;; Log buffer naming should include session when provided
(ert-deftest codex-cli-test--log-buffer-name-per-session ()
  "Test log buffer naming for default and named sessions."
  (should (string= "*codex-cli-log:proj*" (codex-cli--log-buffer-name "proj")))
  (should (string= "*codex-cli-log:proj:dev*" (codex-cli--log-buffer-name "proj" "dev"))))

;; Sessions enumeration for current project
(ert-deftest codex-cli-test--sessions-for-project-detects-named ()
  "Detect named sessions for the current project."
  (let ((b1 (get-buffer-create "*codex-cli:proj-a:abc123*")))
    (unwind-protect
        (cl-letf (((symbol-function 'codex-cli--project-name) (lambda () "proj-a")))
          (let ((sessions (codex-cli--sessions-for-project)))
            (should (listp sessions))
            (should (member "abc123" sessions))))
      (when (buffer-live-p b1) (kill-buffer b1)))))

(ert-deftest codex-cli-test--sessions-for-project-ignores-other-projects ()
  "Do not include sessions from other projects."
  (let ((b1 (get-buffer-create "*codex-cli:proj-a:abc123*"))
        (b2 (get-buffer-create "*codex-cli:proj-b:zzz*")))
    (unwind-protect
        (cl-letf (((symbol-function 'codex-cli--project-name) (lambda () "proj-a")))
          (let ((sessions (codex-cli--sessions-for-project)))
            (should (member "abc123" sessions))
            (should (not (member "zzz" sessions)))))
      (when (buffer-live-p b1) (kill-buffer b1))
      (when (buffer-live-p b2) (kill-buffer b2)))))

(ert-deftest codex-cli-test--sessions-for-project-includes-default ()
  "Include the default session as empty string when present."
  (let ((b1 (get-buffer-create "*codex-cli:proj-a*"))
        (b2 (get-buffer-create "*codex-cli:proj-a:dev*")))
    (unwind-protect
        (cl-letf (((symbol-function 'codex-cli--project-name) (lambda () "proj-a")))
          (let ((sessions (codex-cli--sessions-for-project)))
            (should (member "" sessions))
            (should (member "dev" sessions))))
      (when (buffer-live-p b1) (kill-buffer b1))
      (when (buffer-live-p b2) (kill-buffer b2)))))

;; Global session buffer discovery (cross-project) should find Codex buffers
(ert-deftest codex-cli-test--all-session-buffers ()
  "Ensure we discover Codex buffers and ignore log buffers."
  (let ((b1 (get-buffer-create "*codex-cli:proj*"))
        (b2 (get-buffer-create "*codex-cli:proj:dev*"))
        (log (get-buffer-create "*codex-cli-log:proj*")))
    (unwind-protect
        (let ((all (codex-cli--all-session-buffers)))
          (should (memq b1 all))
          (should (memq b2 all))
          (should (not (memq log all))))
      (when (buffer-live-p b1) (kill-buffer b1))
      (when (buffer-live-p b2) (kill-buffer b2))
      (when (buffer-live-p log) (kill-buffer log)))))

;; Rename session should update buffer and log names
(ert-deftest codex-cli-test--rename-session-buffer-and-log ()
  "Rename a named session; buffer and log buffers should be updated."
  (let* ((b (get-buffer-create "*codex-cli:proj-a:dev*"))
         (l (get-buffer-create "*codex-cli-log:proj-a:dev*")))
    (unwind-protect
        (cl-letf (((symbol-function 'codex-cli--project-name) (lambda () "proj-a")))
          (codex-cli-rename-session "dev" "feature")
          (should (get-buffer "*codex-cli:proj-a:feature*"))
          (should (get-buffer "*codex-cli-log:proj-a:feature*"))
          (should (not (get-buffer "*codex-cli:proj-a:dev*")))
          (should (not (get-buffer "*codex-cli-log:proj-a:dev*"))))
      (dolist (buf (list b l
                         (get-buffer "*codex-cli:proj-a:feature*")
                         (get-buffer "*codex-cli-log:proj-a:feature*")))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;; Rename should use current codex-cli session buffer when called from it
(ert-deftest codex-cli-test--rename-current-session-buffer ()
  "When invoked in a codex-cli buffer, rename that buffer without choosing."
  (let* ((b (get-buffer-create "*codex-cli:proj-x:dev*")))
    (unwind-protect
        (cl-letf (((symbol-function 'codex-cli--project-name) (lambda () "proj-x")))
          (with-current-buffer b
            (codex-cli-rename-session nil "work"))
          (should (get-buffer "*codex-cli:proj-x:work*"))
          (should (not (get-buffer "*codex-cli:proj-x:dev*"))))
      (when (buffer-live-p b) (kill-buffer b))
      (when (get-buffer "*codex-cli:proj-x:work*")
        (kill-buffer "*codex-cli:proj-x:work*")))))

(provide 'codex-cli-test)

;;; codex-cli-test.el ends here
