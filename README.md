# Emacs Config (macOS + Debian)

A modern, fast Emacs configuration with async loading, LSP support, and a yazi-like Dired/Dirvish workflow.

![Screenshot](https://raw.githubusercontent.com/binbinsh/emacs-config/main/screenshot.png)

## Quick Install

```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/binbinsh/emacs-config/main/bootstrap.sh)"
```

`bootstrap.sh` also:
- pre-installs Emacs packages by parsing all `use-package` declarations from `init.el` and `post-init.el`
- byte-compiles all installed ELPA packages, then byte-compiles `early-init.el`, `init.el`, and `post-init.el`

## Features

| Area | Highlights |
|------|------------|
| **Performance** | Async loading via `post-init.el`, native compilation, `gcmh` for smooth GC, fast startup |
| **UI/UX** | Monokai Pro Octagon theme, JetBrainsMono Nerd Font, tabs, smooth scrolling, line numbers, 120-column guide, posframe UIs |
| **Modeline** | `doom-modeline` + `keycast` always-on feedback |
| **Completion** | Vertico + Orderless + Marginalia + Consult + Embark + Corfu + Cape + kind-icon |
| **Explorer** | Treemacs sidebar + Dired/Dirvish panel with yazi-style workflow (preview, queue, tags, bookmarks, sessions) |
| **Preview Ecosystem** | Image/video/pdf thumbnails, archive listing preview, media metadata preview, content-hash cache, external opener policy |
| **Remote** | TRAMP `sshx` workflow with remote preview (temp local copy strategy for thumbnails/metadata/openers) |
| **Terminal** | Integrated `eat` terminal panel |
| **Git** | Magit integration with repository picker and transient git command panel, plus diff-hl + blamer |
| **Languages** | Tree-sitter grammars (pinned), LSP hooks for Python, JS/TS, Go, Rust, Bash, Dart, JSON, Web |
| **Python** | Pyright LSP, Ruff, format + organize imports on save, pytest via `uv run`, debugpy/dap-mode |
| **LaTeX** | AUCTeX with SyncTeX, doc-view PDF preview |
| **jsonl** | JSONL line pretty preview |

## Structure

```text
~/.emacs.d/
├── early-init.el           # Pre-GUI: GC tuning, UI chrome, package archives (China mirrors)
├── init.el                 # Minimal sync setup, theme, fonts, async trigger
├── post-init.el            # All features load async via idle timer
├── bootstrap.sh            # One-command installer (deps, fonts, grammars, LSP)
├── tree-sitter/            # Pre-compiled grammar libraries (pinned versions)
└── .venv/                  # Python virtual environment (created by bootstrap.sh)
```

## What bootstrap.sh Installs

**macOS (via Homebrew):**
- emacs-app (Homebrew cask, installed to /Applications)
- git stack: `git`
- search/dev tools: `ripgrep`, `fd`, `cmake`, `pkg-config`, `libtool`
- preview stack: `ffmpeg`, `ffmpegthumbnailer`, `poppler`, `p7zip`, `mediainfo`, `exiftool`
- language/tooling: `node` + `npm`, `pandoc`, `ast-grep` (or npm fallback)
- JetBrainsMono Nerd Font, Symbols Nerd Font Mono

**Debian (via apt):**
- emacs, git, ripgrep, fd-find, cmake, build-essential
- preview stack: `ffmpeg`, `ffmpegthumbnailer`, `poppler-utils`, `p7zip-full`, `mediainfo`, `libimage-exiftool-perl`
- language/tooling: `nodejs`, `npm`, `pandoc`, `xdg-utils`, `file`
- Noto fonts (CJK, emoji)
- JetBrainsMono Nerd Font, Symbols Nerd Font (downloaded)

**Cross-platform:**
- uv (Python package manager)
- bash-language-server (via npm)
- ast-grep CLI (package manager or npm fallback `@ast-grep/cli`)
- tree-sitter grammars (compiled with pinned versions)
- Python tooling: ruff, ruff-lsp, pyright, debugpy, pytest, black
- Emacs package pre-install from nested `use-package` declarations + full package/config byte-compile during bootstrap

## Keybindings

### Global shortcuts

| Key | Action |
|-----|--------|
| `C-c e` | Focus/open Treemacs sidebar |
| `C-c /` | Ripgrep search |
| `C-c ?` | AST search (ast-grep) |
| `C-c b` | Switch buffers |
| `C-c o` | Open file |
| `C-c j` | Symbols (imenu) |
| `C-c d` | Go to definition |
| `C-c l` | Maximize frame, pick TeX dir + master `.tex`, open TeX/PDF 2-column layout |
| `C-c !` | List diagnostics |
| `C-c v` | Toggle `eat` terminal panel |
| `C-c g` | Magit dashboard (current repo / picker) |
| `C-c G` | Git command panel (Magit actions) |
| `C-x g` | Magit dashboard |
| `C-c u` | Undo tree (vundo) |
| `C-c [` | Previous tab |
| `C-c ]` | Next tab |
| `C-c n` | New tab |
| `C-c x` | Close tab |
| `C-c h` | SSHX remote dired |

### Dired/Dirvish workflow keys

| Key | Action |
|-----|--------|
| `C-c ;` | Unified file manager panel (preview/workflow/state) |
| `C-c ,` | Dirvish dispatch |
| `C-c i` | Metadata preview |
| `C-c T` | Thumbnail preview |
| `C-c q` | Realtime operation queue + progress bars |
| `C-c S` | Restore saved session |
| `C-c k` | Bookmark add |
| `C-c K` | Bookmark jump |
| `C-c A` | Add tag |
| `C-c R` | Remove tag |
| `C-c z` | Open tagged files |
| `C-c H` | Toggle hidden files |
| `C-c r` | Cycle sort (name/time/size) |
| `C-c y` | Yank files |
| `C-c X` | Cut files |
| `C-c P` | Paste files (conflict prompt) |
| `C-c O` | Cycle external opener policy (`ext-only`/`always`/`never`) |
| `C-c W` | Bulk rename (wdired) |
| `!` | Open selected files externally (policy-aware, TRAMP-safe) |

`C-c ;` opens the unified command panel; inside panel:
- `l` toggle live preview
- `c` prune preview cache (TTL + max entries)
- `C` clear preview cache and hash index

### Mode-specific

| Mode | Key | Action |
|------|-----|--------|
| Markdown (`.md`) | `C-c p` | Preview Markdown (`markdown-preview`) |

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
| `s-\`` | Toggle terminal |
| `s-E` | Focus explorer |
| `s-G` | Magit dashboard |
| `s-d` | Mark next like this (multiple cursors) |

## Dependencies

### Required
- Emacs 30+
- `git`, `ripgrep`
- `fd` (or `fd-find`; bootstrap will link `fd` -> `fdfind` on Debian when needed)
- `magit` Emacs package (installed by bootstrap package pre-install)
- `eat` Emacs package
- `ast-grep` CLI (`ast-grep` or `sg`)
- Linux external opener: `xdg-open` or `gio`

### Optional but recommended
- `ffmpegthumbnailer`, `ffmpeg` (video thumbnails)
- `poppler-utils` / `poppler` (`pdftoppm` for PDF thumbnail preview)
- `mediainfo`, `exiftool` (rich metadata preview)
- `p7zip` / `p7zip-full` (archive preview)
- Node.js + npm (bash-language-server)
- `pandoc` (markdown preview)

## Cache Location

All temporary files go to `~/.cache/emacs/` (XDG compliant):
- auto-save, backups, eln-cache, lsp, tramp, treemacs, svg-lib, transient, etc.
- Dired preview cache: `~/.cache/emacs/dirvish/preview-cache/`
- Dired preview hash index/session/tags: `~/.cache/emacs/dirvish/preview-hash-index.el`, `session.el`, `tags.el`

## Package Archives

Uses China mirrors (TUNA) for faster package downloads:
- GNU ELPA, NonGNU ELPA, MELPA
