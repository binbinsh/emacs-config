#!/usr/bin/env bash

set -euo pipefail

REPO_DIR="$HOME/.emacs.d"
REPO_URL="https://github.com/binbinsh/emacs-config.git"

log() { printf "\033[1;32m[✔]\033[0m %s\n" "$*"; }
info() { printf "\033[1;34m[i]\033[0m %s\n" "$*"; }
warn() { printf "\033[1;33m[!]\033[0m %s\n" "$*"; }
err() { printf "\033[1;31m[✘]\033[0m %s\n" "$*"; }

have() { command -v "$1" >/dev/null 2>&1; }

ensure_path() {
  case :"$PATH": in
    *:"$1":*) ;;
    *) export PATH="$1:$PATH" ;;
  esac
}

OS="$(uname -s)"

install_macos() {
  info "Detected macOS"
  # Ensure Homebrew
  if ! have brew; then
    warn "Homebrew not found. Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    # Add default Apple Silicon path; Intel will usually be in /usr/local/bin
    [ -d "/opt/homebrew/bin" ] && ensure_path "/opt/homebrew/bin"
    [ -d "/usr/local/bin" ] && ensure_path "/usr/local/bin"
  fi

  brew install emacs-app

  # CLI deps
  brew install git ripgrep fd cmake pkg-config libtool git-delta || true

  log "macOS base dependencies installed"
}

install_ubuntu() {
  info "Detected Ubuntu/Linux"
  if ! have apt; then
    err "apt not found. This installer targets Ubuntu/Debian."
    exit 1
  fi
  sudo apt update -y
  sudo apt install -y \
    emacs git ripgrep fd-find cmake build-essential pkg-config libtool-bin \
    libvterm-dev xclip curl fonts-noto fonts-noto-cjk fonts-noto-color-emoji

  # Provide `fd` alias if only fdfind exists
  if have fdfind && ! have fd; then
    mkdir -p "$HOME/.local/bin"
    ln -sf "$(command -v fdfind)" "$HOME/.local/bin/fd"
    ensure_path "$HOME/.local/bin"
  fi

  log "Ubuntu base dependencies installed"
}

install_uv() {
  if ! have uv; then
    info "Installing uv (Python package manager)"
    curl -LsSf https://astral.sh/uv/install.sh | sh
  fi
  ensure_path "$HOME/.local/bin"
  log "uv ready: $(uv --version || true)"
}

ensure_repo() {
  if [ -d "$REPO_DIR/.git" ]; then
    info "Updating existing repo at $REPO_DIR"
    git -C "$REPO_DIR" pull --ff-only || true
  else
    info "Cloning repo to $REPO_DIR"
    git clone "$REPO_URL" "$REPO_DIR"
  fi
}

setup_python() {
  info "Setting up Python tooling via uv in $REPO_DIR"
  cd "$REPO_DIR"
  if [ ! -d .venv ]; then
    uv venv --python 3.12 || uv venv
  fi
  VENV_PY="$REPO_DIR/.venv/bin/python"
  if [ -x "$VENV_PY" ]; then
    uv pip install --python "$VENV_PY" \
      ruff ruff-lsp pyright debugpy pytest black
  else
    warn "Virtualenv python missing; skipping pip installs"
  fi
  log "Python tooling installed"
}

prewarm_fonts() {
  info "Installing all-the-icons fonts (non-interactive)"
  emacs -Q --batch --eval \
    '(progn (require (quote package)) (package-initialize) (unless (package-installed-p (quote all-the-icons)) (package-refresh-contents) (package-install (quote all-the-icons))) (require (quote all-the-icons)) (all-the-icons-install-fonts t))' \
    || warn "Font installation encountered issues (you can rerun inside Emacs: M-x all-the-icons-install-fonts)"
}

main() {
  case "$OS" in
    Darwin) install_macos ;;
    Linux) install_ubuntu ;;
    *) err "Unsupported OS: $OS"; exit 1 ;;
  esac
  install_uv
  ensure_repo
  setup_python
  prewarm_fonts

  cat <<'EOF'

Done!

Next steps:
- Launch Emacs and enjoy. On Ubuntu, ensure an emoji/CJK-capable font is selected (Noto packages installed).
- Terminal (vterm) will auto-build if needed; we installed cmake/libtool/libvterm.
- If `fd` command is missing, we created a symlink to `fdfind` in ~/.local/bin.

Tip: Re-run this installer any time; it is safe and idempotent.

EOF
}

main "$@"
