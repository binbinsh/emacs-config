# Emacs Config (Homebrew macOS + Linux)

![Screenshot](https://raw.githubusercontent.com/binbinsh/emacs-config/main/screenshot.png)

## Introduction

This is a lightweight Emacs setup focused on speed and daily workflow quality.

- Fast startup via staged async loading
- File/project workflows with Treemacs and yazi-like Dired/Dirvish
- Fast editing with LSP, rich completion, and preview tooling

## Install

```bash
git clone https://github.com/binbinsh/emacs-config.git ~/.emacs.d
~/.emacs.d/bootstrap.sh
```

If the repo is already present, re-run `~/.emacs.d/bootstrap.sh` only.

`bootstrap.sh` is now Homebrew-first on macOS and keeps the existing `apt` flow on Debian/Linux.

## Function: Performance and UI

- Startup is optimized with staged module loading from `post-init.el`
- GC tuning and native compilation support
- Optional UX polish: doom-modeline, keycast, smooth scrolling, line numbers, column guide

## Function: File and Project Management (Treemacs, Dired, Dirvish)

- Treemacs for project sidebar navigation
- yazi-inspired Dired/Dirvish workflow with preview, bookmarks, tags, queues, and sessions

| Key | Action |
|-----|--------|
| `C-c e` | Focus/open Treemacs |
| `C-c ;` | Open unified file-manager panel |
| `C-c ,` | Dirvish dispatch |
| `C-c a` | Dirvish quick access |
| `C-c f` | Dirvish fd search in current Dired/Dirvish directory, including VCS-ignored paths |
| `C-c t` | Toggle Dirvish subtree |
| `C-c E` | Dirvish emerge menu |
| `C-c i` | Show metadata preview |
| `C-c T` | Generate thumbnail preview |
| `C-c q` | Open operation queue |
| `C-c k` | Add bookmark |
| `C-c K` | Jump bookmark |
| `C-c A` | Add tag |
| `C-c R` | Remove tag |
| `C-c z` | Open files by tag |
| `C-c S` | Restore session |
| `C-c H` | Toggle hidden files |
| `C-c r` | Cycle sort order |
| `C-c y` | Yank files |
| `C-c X` | Cut files |
| `C-c P` | Paste files |
| `C-c O` | Cycle external opener policy |
| `C-c W` | Bulk rename (wdired) |
| `C-c Y` | Copy current path |
| `C-c m` | Open yazi-style session |
| `V` | Toggle quick preview |
| `!` | Open files via external app |
| `C-x d` | Dirvish DWIM (available immediately at startup) |
| `)` | Toggle dired-git-info |
| `RET` | Open current item |
| `C` | Clear preview cache/hash index |
| `c` | Prune panel preview cache entries |
| `l` | Toggle live preview in panel |

## Function: Search and Navigation

| Key | Action |
|-----|--------|
| `C-c /` | project-wide ripgrep |
| `C-c ?` | ast-grep search |
| `C-c b` | switch buffers |
| `C-c o` | open file |
| `C-c j` | symbol/imenu jump |
| `C-c d` | go to definition |
| `C-c !` | open diagnostics |
| `s-F` | project search (dwim) |
| `s-P` | command palette (`M-x`) |
| `s-p` | find file in project |
| `C-c B` | run project compile |
| `C-c w` | toggle global soft wrap |

## Function: Git Workflow

| Key | Action |
|-----|--------|
| `C-c g` | Open Magit dashboard |
| `s-G` | Open Magit dashboard |
| `C-x g` | Open Magit dashboard |
| `C-c G` | Open git action panel |
| `C-c [h` | Previous git hunk |
| `C-c ]h` | Next git hunk |
| `C-c =` | Show current hunk diff |

## Function: Terminal

| Key | Action |
|-----|--------|
| `C-c v` | Toggle `eat` terminal panel |
| `s-\`` | Toggle `eat` terminal panel |

## Function: Remote (TRAMP / SSHX)

| Key | Action |
|-----|--------|
| `C-c h` | Open remote directory via `sshx` |

## Function: Editing and Language Features

- LSP, consult, embark, corfu/cape, treesit are wired for language work
- TypeScript / TSX gets `typescript-ts-mode` + `tsx-ts-mode`, `typescript-language-server`, project-local `tsserver` preference, inlay hints, and save-time import organization + formatting
- Helpful / which-key / docs and navigation helpers integrated

| Key | Scope | Action |
|-----|-------|--------|
| `C-c r` | `lsp-mode` | Find references |
| `C-c .` | `lsp-mode` | Execute code action |
| `C-c f` | `lsp-mode` | Format buffer |
| `C-c i` | `lsp-mode` | Organize imports |
| `C-c t` | `python-mode` | Run pytest via uv |
| `C-c >` | global | Mark next-like-this |
| `s-d` | global | Mark next-like-this |

## Function: Markdown, LaTeX and Document Preview

- Markdown preview using pandoc
- LaTeX workflow with AUCTeX (2-column TeX/PDF workflow)
- JSONL live preview mode

| Key | Action |
|-----|--------|
| `C-c p` | Markdown preview |
| `C-c l` | Open TeX + PDF split preview |

## Function: General Helpers and Window/Tab Shortcuts

| Key | Action |
|-----|--------|
| `C-h F` | Helpful function docs |
| `C-h C` | Helpful command docs |
| `s-c` | Copy |
| `s-x` | Cut |
| `s-v` | Paste |
| `C-c [` | Previous tab |
| `C-c ]` | Next tab |
| `C-c n` | New tab |
| `C-c x` | Close tab |
| `C-c u` | Undo timeline (vundo) |

## Structure

```text
~/.emacs.d/
├── early-init.el           # pre-GUI init: cache, UI chrome, package archives
├── init.el                 # minimal sync init + async trigger
├── post-init.el            # async feature loading
├── bootstrap.sh            # one-command setup helper
├── themes/                 # repo-local custom themes
├── tree-sitter/            # precompiled tree-sitter grammars
└── .venv/                  # optional local Python env
```

## Dependencies

### macOS
- Homebrew
- `git`

Homebrew is the expected package manager on macOS for Emacs, `rg`, `fd`, `uv`, `ast-grep`, `pandoc`, preview helpers, the Node-based language servers, and `op` (`1password-cli`) when you use 1Password.

### Linux
- Emacs 30+
- `git`, `ripgrep`
- `fd` or `fd-find`
- `ast-grep` CLI (`ast-grep` or `sg`)
- `magit`, `eat` (via package install)
- `xdg-open` or `gio` (Linux for external opening)

### Optional but recommended
- `ffmpegthumbnailer`, `ffmpeg`
- `poppler-utils` / `poppler`
- `mediainfo`, `exiftool`
- `p7zip` / `p7zip-full`
- `nodejs`, `npm`
- `typescript`, `typescript-language-server` for deep TypeScript / TSX IDE support
- `pandoc`

## Cache Paths

Temporary and generated files are under `~/.cache/emacs/`:

- auto-save, backups, eln-cache, lsp, tramp, treemacs, svg-lib, transient, etc.
- Dired/Dirvish preview cache: `~/.cache/emacs/dirvish/preview-cache/`
- Dirvish metadata: `~/.cache/emacs/dirvish/preview-hash-index.el`, `session.el`, `tags.el`
