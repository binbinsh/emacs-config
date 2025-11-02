# Emacs Config (macOS + Ubuntu)

### Quick install

```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/binbinsh/emacs-config/main/install-emacs.sh)"
```

### Main features
- **Global AI assistant** on `C-c a`: type a command and it will either execute returned Emacs Lisp or show info. Requires LM Studio API running locally (defaults: `my/lmstudio-base-url`, `my/lmstudio-model` in `lisp/setup-profile.el`).
- **UI/UX**: `Catppuccin` (Latte) theme, JetBrainsMono Nerd Font + CJK/Emoji fonts, `doom-modeline` + `keycast`, tabs, smooth scrolling, line numbers, 120‑column indicator, posframe UIs (`which-key-posframe`, `vertico-posframe`), Helpful/Eldoc‑Box hovers.
- **Completion & search**: Vertico, Orderless, Marginalia, Consult (+ LSP), Embark, Corfu + Cape + popup info, kind‑icon.
- **Explorer & files**: Dirvish sidebar and enhanced Dired (icons, git info, quick preview, two‑pane), `diff-hl` fringe highlights.
- **Large files**: 64MB+ open via VLF chunks; toggle soft wrap with `C-c w`; highlighting optional.
- **Language support**: 20+ languages with Tree-sitter syntax, LSP hooks, and smart navigation (`C-c j` for symbols, `M-.` for definitions, ripgrep fallback).
- **Terminal**: Integrated vterm panel toggle; terminal hub (SSH, remote Dired via TRAMP, tunnels); optional AI command suggestions via LM Studio.
- **Git**: Magit workflow, inline blame overlays, delta‑enhanced diffs, optional AI commit messages.
- **Python**: LSP (pyright), Ruff LSP via `uv run`, format + organize imports on save, pytest via `uv run`, debugging via `dap-mode`/debugpy.
- **Markdown**: Auto live preview in a right split (xwidget‑webkit or EWW).
- **Snippets**: Yasnippet with Consult‑powered searchable selection (`C-c s`), global activation, personal snippets in `~/.config/emacs/snippets/` (outside this repo).
- **Clipboard & perf**: `simpleclip` clipboard integration; smoother GC via `gcmh`.
- **Gmail with AI**: run `install-emacs.sh`, then `setup-gmail.sh you@gmail.com`; launch Emacs → `C-c m`; archive `e`, star `*`, AI: `C-c l` suggest labels, `C-c r` draft reply, `C-c s` summarize.

### Keys
- All keybindings are centralized in `lisp/setup-keys.el`.
- Global and mode-specific actions use `C-c` single keys.
