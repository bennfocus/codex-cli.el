;; -*- mode: eask; lexical-binding: t; -*-

;; Repositories
(source "gnu")
(source "melpa")

;; Package metadata
(package "codex-cli" "0.1.3" "Run Codex CLI inside Emacs with helpers.")
(website-url "https://github.com/bennfocus/codex-cli.el")
(keywords "tools" "convenience" "codex")
(license "MIT")

;; Main entry and files included in the build/package
(package-file "codex-cli.el")
(files "codex-cli.el" "codex-cli-*.el")

;; Dependencies
;; Note: vterm is optional at runtime; we intentionally do not depend on it.
(depends-on "emacs" "28.1")

;; Scripts
(script "compile"    "eask compile")
(script "byte-compile" "eask compile")
(script "test"       "eask test ert tests/*.el")
(script "lint"       "eask lint checkdoc && eask lint package-lint")
(script "clean"      "eask clean-all")
