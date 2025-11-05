# Emacs Config (macOS + Ubuntu)

### Quick install

```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/binbinsh/emacs-config/main/install-emacs.sh)"
```

### Main features
- **Global AI (beta)**: type a command and it will either execute returned Emacs Lisp or show info. Requires LM Studio API running locally.
- **UI/UX**: `Monokai Pro Light` theme, JetBrainsMono Nerd Font + CJK/Emoji fonts, `doom-modeline` + `keycast`, tabs, smooth scrolling, line numbers, 120‑column indicator. Posframe UIs. Helpful/Eldoc‑Box hovers.
- **Completion & search**: Vertico, Orderless, Marginalia, Consult (+ LSP), Embark, Corfu + Cape + popup info, kind‑icon.
- **Diagnostics**: Flymake (built‑in) via LSP (Consult if available, else Flymake buffer).
- **Explorer & files**: Dirvish sidebar and enhanced Dired (icons, git info, quick preview, two‑pane), `diff-hl` fringe highlights.
- **Large files**: 64MB+ open via VLF chunks; soft wrap toggle available; highlighting optional.
- **Language support**: 20+ languages with Tree-sitter syntax, LSP hooks, and smart navigation (symbols/definitions, ripgrep fallback).
- **Terminal**: Integrated vterm panel toggle; terminal hub (SSH, remote Dired via TRAMP, tunnels); optional AI command suggestions via LM Studio.
- **Git**: Magit workflow, inline blame overlays, delta‑enhanced diffs, optional AI commit messages.
- **Python**: LSP (pyright), Ruff LSP via `uv run`, format + organize imports on save, pytest via `uv run`, debugging via `dap-mode`/debugpy.
- **Markdown**: Auto live preview in a right split (xwidget‑webkit or EWW).
- **Snippets**: Yasnippet with Consult‑powered searchable selection, global activation, personal snippets in `~/.config/emacs/snippets/` (outside this repo).
- **Clipboard & perf**: `simpleclip` clipboard integration; smoother GC via `gcmh`.
- **Gmail with AI**: run `install-emacs.sh`, then `setup-gmail.sh you@gmail.com`; Gmail UI and AI helpers available after launch.


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
| `C-c g` | Magit status |
| `C-c y` | Show inline commit info |
| `C-c l` | Toggle inline blame |
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

