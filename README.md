# Emacs Config (macOS + Ubuntu)

A modern, fast Emacs configuration with async loading, LSP support, and VSCode-like UX.

## Quick Install

```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/binbinsh/emacs-config/main/bootstrap.sh)"
```

## Features

| Area | Highlights |
|------|------------|
| **Performance** | Async loading via `post-init.el`, native compilation, `gcmh` for smooth GC, fast startup |
| **UI/UX** | Monokai Light theme, JetBrainsMono Nerd Font, tabs, smooth scrolling, line numbers, 120-column guide, posframe UIs |
| **Modeline** | `doom-modeline` + `keycast` always-on feedback |
| **Completion** | Vertico + Orderless + Marginalia + Consult + Embark + Corfu + Cape + kind-icon |
| **Code Search** | `ast-grep.el` structural search with AST patterns |
| **Diagnostics** | Flymake via LSP for inline errors. `C-c !` opens diagnostics list |
| **Explorer** | Treemacs sidebar with nerd-icons, auto-start, VSCode-style keybindings |
| **Terminal** | Integrated eat terminal panel |
| **Git** | Magit, gitflow, inline blame (blamer), delta diffs, diff-hl gutter |
| **Languages** | Tree-sitter grammars (pinned to v14), LSP hooks for Python, JS/TS, Go, Rust, Bash, JSON, Web |
| **Python** | Pyright LSP, Ruff, format + organize imports on save, pytest via `uv run`, debugpy/dap-mode |
| **Bash** | bash-language-server LSP, tree-sitter syntax highlighting |
| **Markdown** | Live preview, syntax highlighting, pandoc integration |
| **LaTeX** | AUCTeX with SyncTeX, doc-view PDF preview |
| **Clipboard** | `simpleclip` keeps macOS/Linux clipboards synced |

## Structure

```
~/.emacs.d/
├── early-init.el           # Pre-GUI: GC tuning, UI chrome, package archives (China mirrors)
├── init.el                 # Minimal sync setup, theme, fonts, async trigger
├── post-init.el            # All features load async via idle timer
├── bootstrap.sh            # One-command installer (deps, fonts, grammars, LSP)
├── monokai-light-theme.el  # Custom light theme
├── tree-sitter/            # Pre-compiled grammar libraries (pinned versions)
└── .venv/                  # Python virtual environment (created by bootstrap.sh)
```

## What bootstrap.sh Installs

**macOS (via Homebrew):**
- emacs-plus (symlinked to /Applications)
- git, ripgrep, fd, cmake, pkg-config, libtool, git-delta
- JetBrainsMono Nerd Font, Symbols Nerd Font Mono

**Ubuntu (via apt):**
- emacs, git, ripgrep, fd-find, cmake, build-essential
- Noto fonts (CJK, emoji)
- JetBrainsMono Nerd Font, Symbols Nerd Font (downloaded)

**Cross-platform:**
- uv (Python package manager)
- bash-language-server (via npm)
- Tree-sitter grammars (compiled with pinned versions for stability)
- Python tooling: ruff, ruff-lsp, pyright, debugpy, pytest, black

## Keybindings

### Global shortcuts

| Key | Action |
|-----|--------|
| `C-c e` | Focus/toggle Treemacs sidebar |
| `C-c /` | Ripgrep search |
| `C-c ?` | AST search (ast-grep) |
| `C-c b` | Switch buffers |
| `C-c p` | Project files |
| `C-c o` | Open file |
| `C-c j` | Symbols (imenu) |
| `C-c d` | Go to definition |
| `C-c !` | List diagnostics |
| `C-c v` | Toggle terminal panel |
| `C-c g` | Git repo dashboard |
| `C-c y` | Show inline commit info |
| `C-c u` | Undo tree (vundo) |
| `C-c w` | Toggle soft wrap |
| `C-c [` | Previous tab |
| `C-c ]` | Next tab |
| `C-c n` | New tab |
| `C-c x` | Close tab |
| `C-c B` | Project compile |

### Git hunk navigation

| Key | Action |
|-----|--------|
| `C-c [h` | Previous hunk |
| `C-c ]h` | Next hunk |
| `C-c =` | Show hunk diff |

### macOS shortcuts (Cmd key)

| Key | Action |
|-----|--------|
| `s-F` | Project-wide search (ripgrep) |
| `s-P` | Command palette (M-x) |
| `s-p` | Project find file |
| `s-b` | Toggle explorer |
| `s-`` ` | Toggle terminal |
| `s-E` | Focus explorer |
| `s-G` | Magit status |
| `s-d` | Mark next like this (multiple cursors) |

### Mode-specific shortcuts

| Mode | Key | Action |
|------|-----|--------|
| Treemacs | `a` | New file |
| Treemacs | `A` | New folder |
| Treemacs | `r` | Rename |
| Treemacs | `d` | Delete |
| Treemacs | `m` | Move |
| Treemacs | `c` | Copy file |
| Treemacs | `y` | Copy path |
| Treemacs | `Y` | Copy project root path |
| Treemacs | `o` | Open (ace select window) |
| Treemacs | `O` | Open horizontal split |
| Treemacs | `v` | Open vertical split |
| Treemacs | `u` | Go to parent |
| Treemacs | `x` | Collapse parent |
| Treemacs | `R` | Refresh |
| Treemacs | `H` | Toggle hidden files |
| Treemacs | `P` | Peek mode |
| Treemacs | `w` | Set width |
| Dired/Dirvish | `V` | Quick preview |
| Dired/Dirvish | `C` | Copy |
| Dired/Dirvish | `R` | Rename |
| Dired/Dirvish | `+` | Create directory |
| Dired/Dirvish | `D` | Delete |
| Dired/Dirvish | `TAB` | Switch pane |
| LSP | `C-c r` | Find references |
| LSP | `C-c .` | Code action |
| LSP | `C-c f` | Format buffer |
| LSP | `C-c i` | Organize imports |
| Python | `C-c t` | Run pytest |
| Markdown | `C-c l` | Live preview |
| Magit | `C-c b` | Branch graph |
| Magit | `C-c h` | File history |
| Magit | `C-c B` | Toggle blame |

## Dependencies

### Required
- Emacs 29+ (for tree-sitter support)
- git, ripgrep

### Optional
- Node.js + npm (for bash-language-server)
- pandoc (for markdown preview)
- delta (for better git diffs)
- ast-grep (CLI required by `ast-grep.el`)

## Cache Location

All temporary files go to `~/.cache/emacs/` (XDG compliant):
- auto-save, backups, eln-cache, lsp, tramp, treemacs, svg-lib, transient, etc.

## Package Archives

Uses China mirrors (TUNA) for faster package downloads:
- GNU ELPA, NonGNU ELPA, MELPA
