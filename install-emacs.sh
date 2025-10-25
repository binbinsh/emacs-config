#!/usr/bin/env bash
set -euo pipefail

OS="$(uname -s)"
export PATH="$HOME/.local/bin:$PATH"

log() { printf "[install] %s\n" "$*"; }

install_uv() {
  if ! command -v uv >/dev/null 2>&1; then
    curl -LsSf https://astral.sh/uv/install.sh | sh
  fi
}

if [[ "$OS" == "Darwin" ]]; then
  log "Ensuring Homebrew present..."
  if ! command -v brew >/dev/null 2>&1; then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi
  if [[ -x /opt/homebrew/bin/brew ]]; then eval "$(/opt/homebrew/bin/brew shellenv)"; fi
  if [[ -x /usr/local/bin/brew ]]; then eval "$(/usr/local/bin/brew shellenv)"; fi
  log "Installing macOS packages..."
  brew update
  brew install --cask emacs || brew install emacs || true
  brew install git ripgrep git-delta cmake libtool pkg-config libvterm zsh || true
  install_uv
elif [[ "$OS" == "Linux" ]]; then
  export DEBIAN_FRONTEND=noninteractive
  log "Installing Ubuntu packages..."
  sudo apt-get update -y
  sudo apt-get install -y curl ca-certificates git emacs ripgrep cmake build-essential pkg-config libtool-bin libvterm-dev zsh \
    fonts-noto-cjk fonts-noto-color-emoji fonts-dejavu-core fonts-firacode fonts-jetbrains-mono
  sudo apt-get install -y git-delta || sudo apt-get install -y delta || true
  install_uv
else
  echo "Unsupported OS: $OS" >&2
  exit 1
fi

REPO_URL="https://github.com/binbinsh/emacs-config.git"
TARGET_DIR="$HOME/.emacs.d"

log "Setting up Emacs config in $TARGET_DIR"
if [[ -d "$TARGET_DIR/.git" ]]; then
  origin_url="$(git -C "$TARGET_DIR" remote get-url origin || echo)"
  if [[ "$origin_url" == *"binbinsh/emacs-config"* ]]; then
    git -C "$TARGET_DIR" pull --ff-only
  else
    ts=$(date +%Y%m%d-%H%M%S)
    mv "$TARGET_DIR" "$TARGET_DIR.bak-$ts"
    git clone --depth 1 "$REPO_URL" "$TARGET_DIR"
  fi
elif [[ -d "$TARGET_DIR" ]]; then
  ts=$(date +%Y%m%d-%H%M%S)
  mv "$TARGET_DIR" "$TARGET_DIR.bak-$ts"
  git clone --depth 1 "$REPO_URL" "$TARGET_DIR"
else
  git clone --depth 1 "$REPO_URL" "$TARGET_DIR"
fi

cd "$TARGET_DIR"
log "Creating Python venv with uv..."
uv venv --python 3.12
set +u
source .venv/bin/activate
set -u
log "Installing Python tools into the venv..."
uv pip install ruff ruff-lsp pyright pytest debugpy

log "Done. Launch Emacs to complete package installation."
