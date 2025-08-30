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

(ert-deftest codex-cli-test--dummy ()
  "Placeholder test to ensure ERT harness works."
  (should t))

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

(provide 'codex-cli-test)

;;; codex-cli-test.el ends here