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

  # Install Emacs with xwidgets (WebKit) via emacs-plus tap
  brew tap d12frosted/emacs-plus >/dev/null 2>&1 || true
  if ! brew list emacs-plus >/dev/null 2>&1; then
    info "Installing emacs-plus with xwidgets"
    brew install emacs-plus --with-xwidgets || warn "Failed to install emacs-plus with xwidgets"
  else
    info "emacs-plus already installed"
  fi

  # Symlink Emacs.app into /Applications for Spotlight/Launchpad
  brew_prefix="$(brew --prefix)"
  app_src="$brew_prefix/opt/emacs-plus/Emacs.app"
  if [ ! -d "$app_src" ]; then
    app_src="$(ls -d "$brew_prefix"/opt/emacs-plus@*/Emacs.app 2>/dev/null | head -n1 || true)"
  fi
  if [ -d "$app_src" ]; then
    ln -sfn "$app_src" /Applications/Emacs.app || true
  else
    warn "Emacs.app not found under emacs-plus prefix; skip /Applications symlink"
  fi

  # Mail tooling
  brew install notmuch || true

  # CLI deps
  brew install git ripgrep fd cmake pkg-config libtool git-delta || true

  # Nerd Fonts (JetBrainsMono)
  brew tap homebrew/cask-fonts >/dev/null 2>&1 || true
  if ! brew list font-jetbrains-mono-nerd-font >/dev/null 2>&1; then
    brew install font-jetbrains-mono-nerd-font || warn "Failed to install JetBrainsMono Nerd Font via Homebrew"
  fi

  # Nerd Fonts Symbols Only (provides "Symbols Nerd Font Mono" for icons)
  if ! brew list font-symbols-only-nerd-font >/dev/null 2>&1; then
    brew install font-symbols-only-nerd-font || warn "Failed to install Symbols Nerd Font via Homebrew"
  fi

  log "Nerd Font installation step completed"
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
    emacs notmuch libnotmuch-dev git ripgrep fd-find cmake build-essential pkg-config libtool-bin \
    libvterm-dev xclip curl unzip fontconfig fonts-noto fonts-noto-cjk fonts-noto-color-emoji

  # Provide `fd` alias if only fdfind exists
  if have fdfind && ! have fd; then
    mkdir -p "$HOME/.local/bin"
    ln -sf "$(command -v fdfind)" "$HOME/.local/bin/fd"
    ensure_path "$HOME/.local/bin"
  fi

  # Nerd Fonts (JetBrainsMono)
  dest="$HOME/.local/share/fonts/NerdFonts/JetBrainsMono"
  mkdir -p "$dest"
  if ! ls "$dest"/*.ttf >/dev/null 2>&1; then
    tmp_dir="$(mktemp -d)"
    if curl -fsSL -o "$tmp_dir/JetBrainsMono.zip" "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/JetBrainsMono.zip"; then
      unzip -o "$tmp_dir/JetBrainsMono.zip" -d "$dest" >/dev/null 2>&1 || warn "Unzip JetBrainsMono Nerd Font failed"
    else
      warn "Download JetBrainsMono Nerd Font failed"
    fi
    rm -rf "$tmp_dir"
  fi
  fc-cache -f >/dev/null 2>&1 || true

  # Nerd Fonts Symbols Only (provides "Symbols Nerd Font Mono" for icons)
  dest_sym="$HOME/.local/share/fonts/NerdFonts/SymbolsOnly"
  mkdir -p "$dest_sym"
  if ! ls "$dest_sym"/*.ttf >/dev/null 2>&1; then
    tmp_dir="$(mktemp -d)"
    if curl -fsSL -o "$tmp_dir/NerdFontsSymbolsOnly.zip" "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/NerdFontsSymbolsOnly.zip"; then
      unzip -o "$tmp_dir/NerdFontsSymbolsOnly.zip" -d "$dest_sym" >/dev/null 2>&1 || warn "Unzip Nerd Fonts Symbols Only failed"
    else
      warn "Download Nerd Fonts Symbols Only failed"
    fi
    rm -rf "$tmp_dir"
  fi
  fc-cache -f >/dev/null 2>&1 || true

  log "Nerd Font installation step completed"
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

install_lieer() {
  info "Installing lieer (gmi) via uv tool"
  # Ensure uv exists and on PATH
  install_uv
  # Install or upgrade lieer (places entrypoints in ~/.local/bin)
  uv tool install --upgrade lieer || uv tool install lieer
  ensure_path "$HOME/.local/bin"
  log "gmi ready: $(gmi -h || true)"
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

main() {
  case "$OS" in
    Darwin) install_macos ;;
    Linux) install_ubuntu ;;
    *) err "Unsupported OS: $OS"; exit 1 ;;
  esac
  install_uv
  install_lieer
  ensure_repo
  setup_python

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
