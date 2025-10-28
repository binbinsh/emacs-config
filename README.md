# Emacs Config (macOS + Ubuntu)

Quick install (URL):

```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/binbinsh/emacs-config/main/install-emacs.sh)"
```

Notes:
- macOS installs Emacs via Homebrew emacs-plus (GUI) and common CLI deps.
- Ubuntu installs `emacs` (GUI) plus ripgrep, fd, cmake, libvterm, etc.
- Python tools are managed with uv in `~/.emacs.d/.venv`.

Key hints:
- Command leader: press Super-; then key (e.g., `m` for M-x, `e` explorer, `` ` `` vterm).
- Toggle terminal panel: Super-`.
- Toggle explorer: Super-b.

Troubleshooting:
- If `fd` is missing on Ubuntu, the installer symlinks `fdfind` to `~/.local/bin/fd`.
- If icons look wrong, run inside Emacs: `M-x all-the-icons-install-fonts`.
- vterm needs cmake/libtool and libvterm; the installer sets these up.


