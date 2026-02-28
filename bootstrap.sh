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

install_brew_packages() {
  local pkg
  for pkg in "$@"; do
    if brew list "$pkg" >/dev/null 2>&1; then
      info "brew package already installed: $pkg"
    else
      info "Installing brew package: $pkg"
      brew install "$pkg" || warn "Failed to install brew package: $pkg"
    fi
  done
}

install_apt_packages() {
  local pkg
  for pkg in "$@"; do
    if dpkg -s "$pkg" >/dev/null 2>&1; then
      info "apt package already installed: $pkg"
    else
      info "Installing apt package: $pkg"
      sudo apt install -y "$pkg" || warn "Failed to install apt package: $pkg"
    fi
  done
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

  # Install Emacs app (Homebrew cask)
  if ! brew list --cask emacs-app >/dev/null 2>&1; then
    info "Installing emacs-app cask"
    brew install --cask emacs-app || warn "Failed to install emacs-app cask"
  else
    info "emacs-app already installed"
  fi

  # emacs-app cask handles /Applications linkage automatically.

  # CLI deps: search, git, and preview toolchain
  install_brew_packages \
    git ripgrep fd cmake pkg-config libtool \
    ffmpeg ffmpegthumbnailer poppler p7zip exiftool mediainfo \
    node pandoc ast-grep

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

install_debian() {
  info "Detected Debian/Linux"
  if ! have apt; then
    err "apt not found. This installer targets Debian-based systems."
    exit 1
  fi
  sudo apt update -y
  install_apt_packages \
    emacs git ripgrep fd-find cmake build-essential pkg-config libtool-bin \
    wl-clipboard curl unzip fontconfig fonts-noto fonts-noto-cjk fonts-noto-color-emoji \
    nodejs npm xdg-utils file pandoc \
    ffmpeg ffmpegthumbnailer poppler-utils \
    p7zip-full mediainfo libimage-exiftool-perl

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
  log "Debian base dependencies installed"
}

install_uv() {
  if ! have uv; then
    info "Installing uv (Python package manager)"
    curl -LsSf https://astral.sh/uv/install.sh | sh
  fi
  ensure_path "$HOME/.local/bin"
  log "uv ready: $(uv --version || true)"
}

install_bash_lsp() {
  info "Installing bash-language-server"
  if have npm; then
    npm install -g bash-language-server || warn "Failed to install bash-language-server via npm"
    log "bash-language-server ready: $(bash-language-server --version 2>/dev/null || echo 'installed')"
  else
    warn "npm not found. Install Node.js to enable bash-language-server."
    warn "  macOS: brew install node"
    warn "  Debian: sudo apt install nodejs npm"
  fi
}

install_ast_grep_cli() {
  info "Ensuring ast-grep CLI is available"
  if have ast-grep || have sg; then
    info "ast-grep CLI already available"
    return 0
  fi
  if have npm; then
    npm install -g @ast-grep/cli || warn "Failed to install @ast-grep/cli via npm"
  else
    warn "npm not found; ast-grep CLI not installed"
  fi
}

verify_emacs_version() {
  if ! have emacs; then
    warn "emacs command not found in PATH; skip version check"
    return 0
  fi

  local major
  major="$(emacs --batch --eval '(princ emacs-major-version)' 2>/dev/null || echo 0)"
  if [ -n "$major" ] && [ "$major" -lt 30 ]; then
    warn "Detected Emacs ${major}. This config requires Emacs 30+."
    warn "  Debian tip: use backports/third-party repo, or build Emacs 30+."
  else
    info "Emacs version check passed (>=30)"
  fi
}

bootstrap_emacs_packages_and_compile() {
  info "Pre-installing Emacs packages from use-package declarations"

  local install_lisp
  install_lisp="$(mktemp)"
  cat >"$install_lisp" <<'ELISP'
(require 'package)
(require 'seq)

(setq package-enable-at-startup nil)
(package-initialize)

(defun my/bootstrap-use-package-target (form)
  "Return package symbol to install for use-package FORM, or nil."
  (let* ((name (nth 1 form))
         (plist (nthcdr 2 form))
         (ensure-key (plist-member plist :ensure))
         (ensure (plist-get plist :ensure)))
    (cond
     ((not ensure-key) (and (symbolp name) name))
     ((eq ensure nil) nil)
     ((eq ensure t) (and (symbolp name) name))
     ((symbolp ensure) ensure)
     ((and (consp ensure) (symbolp (car ensure))) (car ensure))
     (t (and (symbolp name) name)))))

(defun my/bootstrap-collect-use-packages-from-form (form)
  "Collect package symbols from nested use-package FORMs."
  (let (packages)
    (cond
     ((and (listp form) (eq (car form) 'use-package))
      (let ((pkg (my/bootstrap-use-package-target form)))
        (when (and pkg (symbolp pkg))
          (push pkg packages))))
     ((consp form)
      (let ((cur form))
        (while (consp cur)
          (setq packages
                (nconc packages
                       (my/bootstrap-collect-use-packages-from-form (car cur))))
          (setq cur (cdr cur)))
        (when cur
          (setq packages
                (nconc packages
                       (my/bootstrap-collect-use-packages-from-form cur)))))))
    packages))

(defun my/bootstrap-collect-use-packages (file)
  "Collect package symbols from nested use-package forms in FILE."
  (let (packages)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (setq packages
                  (nconc packages
                         (my/bootstrap-collect-use-packages-from-form
                          (read (current-buffer))))))
        (end-of-file nil)))
    packages))

(defun my/bootstrap-package-installable-p (pkg)
  "Return non-nil when PKG should be installed from package archives."
  (and (symbolp pkg)
       (not (package-built-in-p pkg))
       (not (locate-library (symbol-name pkg)))))

(let* ((repo (file-name-as-directory (expand-file-name (or (getenv "REPO_DIR") "~/.emacs.d"))))
       (files (list (expand-file-name "init.el" repo)
                    (expand-file-name "post-init.el" repo)))
       (packages (delete-dups (apply #'append (mapcar #'my/bootstrap-collect-use-packages files))))
       (missing (seq-filter (lambda (pkg)
                              (and (my/bootstrap-package-installable-p pkg)
                                   (not (package-installed-p pkg))))
                            packages))
       (failed nil)
       (orig-package-compile (and (fboundp 'package--compile)
                                  (symbol-function 'package--compile))))
  ;; Skip per-package compile during install. We compile all packages in one pass later.
  (when (fboundp 'package--compile)
    (fset 'package--compile (lambda (&rest _args) t)))
  (unwind-protect
      (progn
        (unless (package-installed-p 'use-package)
          (unless package-archive-contents
            (package-refresh-contents))
          (condition-case err
              (package-install 'use-package)
            (error
             (push (format "use-package (%s)" err) failed))))

        (when missing
          (unless package-archive-contents
            (package-refresh-contents))
          (dolist (pkg missing)
            (condition-case err
                (progn
                  (message "Installing package: %s" pkg)
                  (package-install pkg))
              (error
               (push (format "%s (%s)" pkg err) failed)))))

        (when (fboundp 'package-quickstart-refresh)
          (ignore-errors (package-quickstart-refresh)))

        (message "Package bootstrap summary: total=%d missing=%d failed=%d"
                 (length packages) (length missing) (length failed))
        (when failed
          (dolist (item (nreverse failed))
            (message "Failed: %s" item))
          (kill-emacs 2)))
    (when (and (fboundp 'package--compile) orig-package-compile)
      (fset 'package--compile orig-package-compile))))
ELISP

  if env REPO_DIR="$REPO_DIR" emacs -Q --batch -l "$REPO_DIR/early-init.el" -l "$install_lisp"; then
    log "Emacs packages pre-installed"
  else
    warn "Some Emacs packages failed to pre-install; first startup may finish remaining installs"
  fi
  rm -f "$install_lisp"

  info "Byte-compiling all installed Emacs packages + config files"
  local compile_lisp
  compile_lisp="$(mktemp)"
  cat >"$compile_lisp" <<'ELISP'
(require 'package)
(require 'seq)

(setq package-enable-at-startup nil)
(package-initialize)

(let* ((repo (file-name-as-directory (expand-file-name (or (getenv "REPO_DIR") "~/.emacs.d"))))
       (descs (apply #'append (mapcar #'cdr package-alist)))
       (pkg-failed nil)
       (files (list (expand-file-name "early-init.el" repo)
                    (expand-file-name "init.el" repo)
                    (expand-file-name "post-init.el" repo)))
       (cfg-failed nil))
  (dolist (desc descs)
    (let* ((name (symbol-name (package-desc-name desc)))
           (dir (ignore-errors (package-desc-dir desc))))
      (when (file-directory-p dir)
        (condition-case err
            (progn
              (message "Byte-compiling package: %s" name)
              (byte-recompile-directory dir 0 t))
          (error
           (push (format "%s (%s)" name err) pkg-failed))))))
  (dolist (file files)
    (condition-case err
        (progn
          (message "Byte-compiling: %s" file)
          (byte-recompile-file file 0 t))
      (error
       (push (format "%s (%s)" file err) cfg-failed))))
  (if (or pkg-failed cfg-failed)
      (progn
        (dolist (item (nreverse pkg-failed))
          (message "Package compile failed: %s" item))
        (dolist (item (nreverse cfg-failed))
          (message "Config compile failed: %s" item))
        (kill-emacs 2))
    (message "Byte-compile completed: packages=%d config=%d"
             (length descs) (length files))))
ELISP

  if env REPO_DIR="$REPO_DIR" emacs -Q --batch -l "$compile_lisp"; then
    log "Package + config byte-compile completed"
  else
    warn "Config byte-compile reported issues; Emacs can still compile on first startup"
  fi
  rm -f "$compile_lisp"
}

ensure_repo() {
  if [ -d "$REPO_DIR/.git" ]; then
    info "Repo already exists at $REPO_DIR"
  else
    info "Cloning repo to $REPO_DIR"
    git clone "$REPO_URL" "$REPO_DIR"
  fi
}

setup_treesitter() {
  info "Pre-compiling tree-sitter grammars"
  TS_DIR="$REPO_DIR/tree-sitter"
  mkdir -p "$TS_DIR"

  # Use Emacs batch mode to compile all grammars
  emacs --batch --eval "(progn
    (setq treesit-extra-load-path '(\"$TS_DIR\"))
    (setq treesit-language-source-alist
          '((bash \"https://github.com/tree-sitter/tree-sitter-bash\" \"v0.23.3\")
            (python \"https://github.com/tree-sitter/tree-sitter-python\" \"v0.23.6\")
            (javascript \"https://github.com/tree-sitter/tree-sitter-javascript\" \"v0.23.1\")
            (typescript \"https://github.com/tree-sitter/tree-sitter-typescript\" \"v0.23.2\" \"typescript/src\")
            (tsx \"https://github.com/tree-sitter/tree-sitter-typescript\" \"v0.23.2\" \"tsx/src\")
            (json \"https://github.com/tree-sitter/tree-sitter-json\" \"v0.24.8\")
            (go \"https://github.com/tree-sitter/tree-sitter-go\" \"v0.23.4\")
            (rust \"https://github.com/tree-sitter/tree-sitter-rust\" \"v0.23.3\")
            (dart \"https://github.com/ast-grep/tree-sitter-dart\" \"master\")))
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      (unless (treesit-language-available-p lang)
        (message \"Compiling grammar: %s\" lang)
        (treesit-install-language-grammar lang \"$TS_DIR\")))
    (message \"Tree-sitter grammars ready\"))" 2>&1 || warn "Some grammars may have failed to compile"

  log "Tree-sitter grammars compiled to $TS_DIR"
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
    Linux) install_debian ;;
    *) err "Unsupported OS: $OS"; exit 1 ;;
  esac
  verify_emacs_version
  install_uv
  install_bash_lsp
  install_ast_grep_cli
  ensure_repo
  bootstrap_emacs_packages_and_compile
  setup_treesitter
  setup_python

  cat <<'EOF'

Done!

Next steps:
- Launch Emacs and enjoy. On Debian, ensure an emoji/CJK-capable font is selected (Noto packages installed).
- First launch should be near-ready: packages are pre-installed and all installed packages/config were byte-compiled during bootstrap.
- If `fd` command is missing, we created a symlink to `fdfind` in ~/.local/bin.
- For full feature set, verify commands exist: `ffmpegthumbnailer`, `pdftoppm`, `7zz`/`7z`, `mediainfo`, `exiftool`, `pandoc`, `ast-grep`/`sg`.

Tip: Re-run this installer any time; it is safe and idempotent.

EOF
}

main "$@"
