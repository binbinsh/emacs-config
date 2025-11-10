# Emacs Config (macOS + Ubuntu)

### Quick install

```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/binbinsh/emacs-config/main/install-emacs.sh)"
```

### Main features

| Area | Highlights | Shortcuts / extras |
|------|------------|--------------------|
| **Global AI (beta)** | Natural-language commands that either execute returned Emacs Lisp or surface inline guidance. | Requires LM Studio API running locally. |
| **UI/UX** | `Monokai Pro Light`, JetBrainsMono Nerd Font, tabs, smooth scrolling, line numbers, 120-column guide, posframe UIs, Helpful/Eldoc-Box hovers. | `doom-modeline` + `keycast` always-on feedback. |
| **Completion & search** | Vertico + Orderless + Marginalia stack, Consult (+ LSP), Embark, Corfu + Cape overlays, kind-icon hints. | Minibuffer-first workflow across Emacs. |
| **Diagnostics** | Flymake via LSP (Consult if available, else Flymake buffer) for inline errors and quick lists. | `C-c !` opens diagnostics list. |
| **Explorer & files** | Dirvish sidebar, Dired upgrades (icons, git info, two-pane preview), `diff-hl` fringe indicators. | `C-c e` toggles Dirvish instantly. |
| **Large files** | VLF chunked viewer for 64MB+, quick soft-wrap toggle, optional highlighting to keep things snappy. | `C-c w` toggles soft wrap. |
| **Language support** | 20+ Tree-sitter grammars, LSP hooks, symbol/definition jumps, ripgrep fallback for navigation. | Works on macOS + Ubuntu out of the box. |
| **Terminal** | Integrated vterm panel, SSH/TRAMP hub, tunnel monitor, optional AI command suggestions. | `C-c v` toggles vterm, `C-c h` opens SSH hub. |
| **Git** | Fork-style dashboard grouped by folders, Magit command center, Git-flow helpers, optional Git LFS, inline blame, delta diffs, AI commit assistant. | `C-c g` dashboard, `C-c B` inline blame. |
| **Python** | Pyright LSP, Ruff LSP via `uv run`, format + organize imports on save, pytest via `uv run`, debugpy integration through `dap-mode`. | Tooling isolated through `uv`. |
| **Markdown** | Right-split live preview via xwidget-webkit or EWW that refreshes as you type. | Automatic when editing `.md`. |
| **LaTeX workspace** | `C-c l` launches three-column layout (explorer, editor, PDF) with auto TeX detection, save-to-refresh builds. | Purpose-built for academic workflows. |
| **Snippets** | Yasnippet globally enabled with Consult-powered picker, personal library in `~/.config/emacs/snippets/`. | `C-c s` opens snippet selector. |
| **Clipboard & perf** | `simpleclip` keeps macOS/Linux clipboards synced; `gcmh` smooths GC pauses. | Nice feel for huge buffers. |
| **Gmail with AI** | Run `install-emacs.sh` then `setup-gmail.sh you@gmail.com` to unlock Gmail UI plus AI drafting/summarizing helpers. | Mail workspace available after setup. |


### Quick user manual

Global and mode-specific actions use `C-c` single keys.

Global shortcuts

| Key   | Action |
|-------|--------|
| `C-c e` | Focus/toggle Dirvish sidebar |
| `C-c /` | Ripgrep search |
| `C-c b` | Switch buffers |
| `C-c p` | Project files |
| `C-c j` | Symbols (imenu) |
| `C-c d` | Go to definition |
| `C-c r` | Remote Dired (TRAMP) |
| `C-c !` | List diagnostics (consult-flymake) |
| `C-c v` | Toggle vterm panel |
| `C-c h` | SSH (terminal hub) |
| `C-c g` | Fork Git repo dashboard (folder groups, TAB to fold/unfold) |
| `C-c y` | Show inline commit info |
| `C-c l` | Open LaTeX workspace (explorer + editor + PDF preview) |
| `C-c B` | Toggle inline blame |
| `C-c s` | Global snippet selector (shell snippets go to vterm) |
| `C-c a` | Global AI assistant (requires local LM Studio) |
| `C-c w` | Toggle soft wrap (useful for very large files) |
| `C-c m` | Open Gmail three‑pane UI |

Mode-specific shortcuts

| Mode | Key | Action |
|------|-----|--------|
| Dired/Dirvish | `V` | Quick preview |
| Dired/Dirvish | `C` | Copy |
| Dired/Dirvish | `R` | Rename |
| Dired/Dirvish | `+` | Create directory |
| Dired/Dirvish | `D` | Delete |
| Dired/Dirvish | `TAB` | Switch pane |
| vterm | `C-c h` | SSH connect (host profiles) |
| vterm | `C-c r` | Remote Dired (TRAMP) |
| vterm | `C-c t` | List SSH tunnels |
| vterm | `C-c s` | Snippet selector (shell to vterm) |
| LSP | `C-c r` | Find references |
| LSP | `C-c .` | Code action |
| LSP | `C-c f` | Format buffer |
| LSP | `C-c i` | Organize imports |
| Notmuch (search/show) | `e` | Archive |
| Notmuch (search/show) | `*` | Star |
| Notmuch (search/show) | `l` | AI: suggest labels |
| Notmuch (search/show) | `r` | AI: draft reply |
| Notmuch (search/show) | `s` | AI: summarize thread |
