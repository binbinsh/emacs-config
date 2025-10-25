## Quick start

Run on a fresh macOS or Ubuntu machine:

```bash
curl -fsSL https://raw.githubusercontent.com/binbinsh/emacs-config/refs/heads/main/install-emacs.sh | bash
```

What this does:
- Detects OS (macOS via Homebrew, Ubuntu via apt) and installs Emacs and required tools.
- Installs uv and sets up a Python venv in `~/.emacs.d` with `ruff`, `ruff-lsp`, `pyright`, `pytest`, `debugpy`.
- On Ubuntu, installs fonts (`Noto CJK`, `Noto Color Emoji`, `DejaVu`, `FiraCode`, `JetBrains Mono` if available).
- Clones or updates `binbinsh/emacs-config` into `~/.emacs.d`.

Notes:
- Re-running the command is safe; it will update packages and your Emacs config in place.
- If `~/.emacs.d` existed but wasn't this repo, it's backed up to `~/.emacs.d.bak-<timestamp>`.

