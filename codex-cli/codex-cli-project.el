;;; codex-cli-project.el --- Project root discovery for codex-cli -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: codex-cli.el contributors
;; Keywords: tools, processes
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Project root discovery via `project.el` and relative path helpers for
;; codex-cli.

;;; Code:

(require 'project)

(defun codex-cli-project-root ()
  "Return the project root directory or signal an error.
Falls back to `default-directory' when buffer is not visiting a file."
  (let* ((current-dir (or (and buffer-file-name
                               (file-name-directory buffer-file-name))
                          default-directory))
         (project (project-current nil current-dir)))
    (if project
        (project-root project)
      (error "No project found for directory: %s" current-dir))))

(defun codex-cli-relpath (path)
  "Return PATH relative to the project root.
If PATH is not under the project root, return the absolute path."
  (let ((root (codex-cli-project-root)))
    (if (string-prefix-p root path)
        (file-relative-name path root)
      path)))

(provide 'codex-cli-project)

;;; codex-cli-project.el ends here