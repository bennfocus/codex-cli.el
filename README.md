# codex-cli.el

Minimal Emacs integration for **Codex CLI**. It runs the Codex terminal UI inside Emacs per project and gives you a few high‑leverage helpers for sending prompts, regions, and files. No MCP. No diagnostics. No patch reviewer. Small surface, stable defaults.

**Status:** v0.1 (minimal feature set)

---

## Features

- Launch Codex CLI in a project‑scoped terminal buffer
- Side window on the right with fixed width
- Send prompt from minibuffer, active region, or any file under the project
- Smart chunking for large sends with progress messages
- Optional per‑session preamble injected once
- Last‑block resend and optional injection log buffer

Non‑goals for v0.1: MCP tools, Flycheck/Flymake bridges, ediff workflows, complex menus.

---

## Requirements

- Emacs 28 or newer
- **Codex CLI** installed and on PATH (`codex --version` should work)
- Optional: `vterm` for a better terminal; falls back to built‑in `term`

---

## Install

### Option A: Doom/straight (local working tree)

```elisp
(use-package codex-cli
  :load-path "~/code/claude-code-ide/codex-cli"   ;; adjust path to your repo
  :commands (codex-cli-start codex-cli-toggle codex-cli-send-region codex-cli-send-file)
  :init
  (setq codex-cli-executable "codex"
        codex-cli-terminal-backend 'vterm
        codex-cli-width 90))
```

### Option B: Vanilla Emacs

```elisp
(add-to-list 'load-path "~/code/claude-code-ide/codex-cli") ; adjust path
(require 'codex-cli)
```

Optional keybindings:

```elisp
(global-set-key (kbd "C-c x s") #'codex-cli-start)
(global-set-key (kbd "C-c x t") #'codex-cli-toggle)
(global-set-key (kbd "C-c x r") #'codex-cli-restart)
(global-set-key (kbd "C-c x p") #'codex-cli-send-prompt)
```

---

## Quickstart

1. Ensure Codex CLI is available:
   ```bash
   which codex && codex --version
   ```
2. Open a file inside your project in Emacs.
3. `M-x codex-cli-start` to open a `*codex-cli:PROJECT*` buffer on the right.
4. Try:
   - `M-x codex-cli-send-prompt` to paste a message into the terminal
   - Select a region then `M-x codex-cli-send-region`
   - `M-x codex-cli-send-file` to send any file under the project

---

## Commands

- `codex-cli-start` start or focus the session for the current project
- `codex-cli-toggle` show or hide the side window without killing the process
- `codex-cli-restart` kill and start again in the same buffer
- `codex-cli-stop` terminate the process and bury the buffer
- `codex-cli-send-prompt` minibuffer input pasted into the terminal
- `codex-cli-send-region` send active region or whole buffer as a fenced block
- `codex-cli-send-file` pick a project file and send as a fenced block
- `codex-cli-copy-last-block` re‑send the last injected block

---

## Configuration

```elisp
(defgroup codex-cli nil
  "Run Codex CLI inside Emacs with minimal helpers."
  :group 'tools :prefix "codex-cli-")

(defcustom codex-cli-executable "codex" "Path to Codex CLI.")
(defcustom codex-cli-extra-args nil "List of extra args passed to Codex CLI.")
(defcustom codex-cli-side 'right "Side window placement: left or right.")
(defcustom codex-cli-width 90 "Side window width in columns.")
(defcustom codex-cli-terminal-backend 'vterm "Preferred terminal backend: vterm or term.")
(defcustom codex-cli-max-bytes-per-send 8000 "Chunk size for large sends.")
(defcustom codex-cli-session-preamble nil "Optional text to inject once after start.")
(defcustom codex-cli-log-injections t "Mirror injected blocks into a log buffer.")
```

**Content formatting**

The package wraps content with a header and a fenced code block:

````
# File: relative/path/to/file.ext
```<lang>
<content>
```
````

The language tag is guessed from the current major mode or file extension. If unknown, it is omitted.

---

## Development and reload loop

Fast path while editing:

- Open the changed file and `M-x eval-buffer`

Hard reload when you changed many files:

```elisp
(unload-feature 'codex-cli t)
(load (expand-file-name "~/code/claude-code-ide/codex-cli/codex-cli.el") nil 'nomessage)
```

If Emacs reports dependencies are still loaded, unload in reverse order:

```elisp
(mapc (lambda (f) (ignore-errors (unload-feature f t)))
      '(codex-cli-term codex-cli-utils codex-cli-project codex-cli))
(load (expand-file-name "~/code/claude-code-ide/codex-cli/codex-cli.el") nil 'nomessage)
```

Byte‑compile to catch warnings:

```bash
emacs -Q --batch -L ~/code/claude-code-ide/codex-cli \
  -f batch-byte-compile ~/code/claude-code-ide/codex-cli/*.el
```

---

## Tests

If you added ERT tests as suggested in `tasks.md`:

```bash
emacs -Q --batch \
  -L ~/code/claude-code-ide/codex-cli \
  -l ~/code/claude-code-ide/codex-cli/codex-cli.el \
  -l ~/code/claude-code-ide/tests/codex-cli-test.el \
  -f ert-run-tests-batch-and-exit
```

---

## Troubleshooting

- **Codex not found** set `(setq codex-cli-executable "/full/path/to/codex")` and verify `(executable-find codex-cli-executable)`
- **Window size** tweak `(setq codex-cli-width 100)` and restart the session
- **Unicode or paste issues** install `vterm` or set `(setq codex-cli-terminal-backend 'vterm)`
- **Wrong project root** open a file inside the repo or set `default-directory` before starting
- **Preamble runs twice** increase the post‑spawn idle delay in your code a little

---

## Roadmap

- v0.1 minimal (this release)
- v0.2 small hydra or transient for core actions and `.dir-locals.el` preamble support

For the full design, see `spec.md`.

---

## License

MIT unless your project requires a different license. Update `LICENSE` accordingly.

---

## Acknowledgments

Inspired by the excellent editor‑agent integration patterns in the Emacs community. Built as a clean, minimal alternative tailored for Codex CLI.

