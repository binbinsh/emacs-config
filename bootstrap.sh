#!/usr/bin/env bash

set -euo pipefail

log() { printf "\033[1;32m[✔]\033[0m %s\n" "$*"; }
info() { printf "\033[1;34m[i]\033[0m %s\n" "$*"; }
warn() { printf "\033[1;33m[!]\033[0m %s\n" "$*"; }
err() { printf "\033[1;31m[✘]\033[0m %s\n" "$*"; }

have() { command -v "$1" >/dev/null 2>&1; }

default_repo_dir() {
  local source="${BASH_SOURCE[0]:-}"
  local script_dir

  if [ -n "$source" ] && [ "$source" != "bash" ] && [ -e "$source" ]; then
    script_dir="$(cd "$(dirname "$source")" && pwd -P)"
    if [ -d "$script_dir/.git" ] && [ -f "$script_dir/bootstrap.sh" ] && [ -f "$script_dir/init.el" ]; then
      printf '%s\n' "$script_dir"
      return 0
    fi
  fi

  printf '%s\n' "$HOME/.emacs.d"
}

REPO_DIR="${REPO_DIR:-$(default_repo_dir)}"
REPO_URL="${REPO_URL:-https://github.com/binbinsh/emacs-config.git}"

ensure_path() {
  case :"$PATH": in
    *:"$1":*) ;;
    *) export PATH="$1:$PATH" ;;
  esac
}

ensure_homebrew_path() {
  if [ -x /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
  elif [ -x /usr/local/bin/brew ]; then
    eval "$(/usr/local/bin/brew shellenv)"
  fi
}

brew_install_formulae_if_missing() {
  local formula
  local missing=()

  for formula in "$@"; do
    if ! brew list --formula "$formula" >/dev/null 2>&1; then
      missing+=("$formula")
    fi
  done

  if [ "${#missing[@]}" -gt 0 ]; then
    info "Installing Homebrew formulae: ${missing[*]}"
    brew install "${missing[@]}"
  fi
}

brew_install_casks_if_missing() {
  local cask
  local missing=()

  for cask in "$@"; do
    if ! brew list --cask "$cask" >/dev/null 2>&1; then
      missing+=("$cask")
    fi
  done

  if [ "${#missing[@]}" -gt 0 ]; then
    info "Installing Homebrew casks: ${missing[*]}"
    brew install --cask "${missing[@]}"
  fi
}

install_macos() {
  info "Detected macOS"
  ensure_homebrew_path

  if ! have brew; then
    err "Homebrew not found. Install Homebrew first, then re-run bootstrap.sh."
    exit 1
  fi

  brew_install_formulae_if_missing \
    emacs git ripgrep fd cmake pkg-config libtool tree-sitter-cli \
    fontconfig node@22 pandoc ffmpeg ffmpegthumbnailer poppler p7zip \
    media-info exiftool uv bash-language-server typescript \
    typescript-language-server ast-grep zig zls

  brew_install_casks_if_missing 1password-cli

  if brew list --formula node@22 >/dev/null 2>&1; then
    ensure_path "/opt/homebrew/opt/node@22/bin"
    if ! have node; then
      brew link node@22 --force --overwrite >/dev/null 2>&1 || true
    fi
  fi
}

file_needs_refresh() {
  local source="$1"
  local dest="$2"
  [ ! -f "$dest" ] && return 0
  [ "$source" -nt "$dest" ] && return 0
  [ "$(wc -c <"$source")" -ne "$(wc -c <"$dest")" ] && return 0
  return 1
}

install_bundled_sarasa_term_sc_fonts() {
  local source_dir="$REPO_DIR/assets"
  local dest_dir
  local font
  local dest
  local updated=0

  case "$OS" in
    Darwin) dest_dir="$HOME/Library/Fonts" ;;
    Linux) dest_dir="$HOME/.local/share/fonts/emacs-bundled" ;;
    *) return 0 ;;
  esac

  if ! compgen -G "$source_dir/SarasaTermSC-*.ttf" >/dev/null 2>&1; then
    warn "Bundled Sarasa Term SC fonts not found in $source_dir"
    return 0
  fi

  mkdir -p "$dest_dir"
  for font in "$source_dir"/SarasaTermSC-*.ttf; do
    dest="$dest_dir/$(basename "$font")"
    if file_needs_refresh "$font" "$dest"; then
      cp "$font" "$dest"
      updated=1
    fi
  done

  if [ "$updated" -eq 1 ]; then
    if [ "$OS" = Linux ] && have fc-cache; then
      fc-cache -f "$dest_dir" >/dev/null 2>&1 || true
    fi
    log "Bundled Sarasa Term SC fonts synced to $dest_dir"
  else
    info "Bundled Sarasa Term SC fonts already up to date"
  fi
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

ensure_fd_alias() {
  if have fdfind && ! have fd; then
    mkdir -p "$HOME/.local/bin"
    ln -sf "$(command -v fdfind)" "$HOME/.local/bin/fd"
    ensure_path "$HOME/.local/bin"
    info "Linked fdfind to ~/.local/bin/fd"
  fi
}

require_command() {
  local cmd="$1"
  if have "$cmd"; then
    return 0
  fi
  err "Required command not found: $cmd"
  return 1
}

verify_bootstrap_requirements() {
  local missing=0

  require_command git || missing=1
  require_command emacs || missing=1

  if [ "$missing" -ne 0 ]; then
    exit 1
  fi

  have cc || warn "C compiler not found; tree-sitter grammar compilation may fail"
  have cmake || warn "cmake not found; tree-sitter grammar compilation may fail"
}

OS="$(uname -s)"

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
  ensure_fd_alias

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

prepare_host_environment() {
  case "$OS" in
    Linux)
      install_debian
      ;;
    Darwin)
      install_macos
      ;;
    *)
      err "Unsupported OS: $OS"
      exit 1
      ;;
  esac
}

install_uv() {
  if have uv; then
    ensure_path "$HOME/.local/bin"
    log "uv ready: $(uv --version || true)"
    return 0
  fi

  if [ "$OS" = Darwin ] && have brew; then
    brew_install_formulae_if_missing uv
    log "uv ready: $(uv --version || true)"
    return 0
  fi

  if ! have uv; then
    info "Installing uv (Python package manager)"
    curl -LsSf https://astral.sh/uv/install.sh | sh
  fi
  ensure_path "$HOME/.local/bin"
  if have uv; then
    log "uv ready: $(uv --version || true)"
  else
    warn "uv installation did not add `uv` to PATH"
  fi
}

install_bash_lsp() {
  if have bash-language-server; then
    info "bash-language-server already available"
    return 0
  fi

  info "Ensuring bash-language-server is available"
  if [ "$OS" = Darwin ] && have brew; then
    brew_install_formulae_if_missing bash-language-server
    return 0
  fi

  if have npm; then
    npm install -g bash-language-server || warn "Failed to install bash-language-server via npm"
    log "bash-language-server ready: $(bash-language-server --version 2>/dev/null || echo 'installed')"
  else
    warn "npm not found. Install Node.js via Nix or your system package manager to enable bash-language-server."
  fi
}

install_typescript_lsp() {
  if have typescript-language-server && (have tsc || have tsserver); then
    info "TypeScript language tooling already available"
    return 0
  fi

  info "Ensuring TypeScript language tooling is available"
  if [ "$OS" = Darwin ] && have brew; then
    brew_install_formulae_if_missing typescript typescript-language-server
    return 0
  fi

  if have npm; then
    npm install -g typescript typescript-language-server || warn "Failed to install TypeScript language tooling via npm"
    log "typescript-language-server ready: $(typescript-language-server --version 2>/dev/null || echo 'installed')"
  else
    warn "npm not found. Install Node.js via Nix or your system package manager to enable deep TypeScript support."
  fi
}

install_ast_grep_cli() {
  info "Ensuring ast-grep CLI is available"
  if have ast-grep || have sg; then
    info "ast-grep CLI already available"
    return 0
  fi
  if [ "$OS" = Darwin ] && have brew; then
    brew_install_formulae_if_missing ast-grep
    return 0
  fi
  if have npm; then
    npm install -g @ast-grep/cli || warn "Failed to install @ast-grep/cli via npm"
  else
    warn "npm not found; ast-grep CLI not installed"
  fi
}

install_zig_tooling() {
  if have zig && have zls; then
    info "Zig tooling already available"
    return 0
  fi

  info "Ensuring Zig tooling is available"
  if [ "$OS" = Darwin ] && have brew; then
    brew_install_formulae_if_missing zig zls
    return 0
  fi

  warn "Install zig and zls with your system package manager for Zig LSP support."
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
  elif [ -e "$REPO_DIR" ]; then
    err "$REPO_DIR exists but is not a git checkout"
    exit 1
  else
    if ! have git; then
      err "git not found; cannot clone $REPO_URL"
      exit 1
    fi
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
            (zig \"https://github.com/tree-sitter-grammars/tree-sitter-zig\" \"v1.1.2\")
            (dart \"https://github.com/ast-grep/tree-sitter-dart\" \"master\")))
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      (unless (treesit-language-available-p lang)
        (message \"Compiling grammar: %s\" lang)
        (treesit-install-language-grammar lang \"$TS_DIR\")))
    (message \"Tree-sitter grammars ready\"))" 2>&1 || warn "Some grammars may have failed to compile"

  log "Tree-sitter grammars compiled to $TS_DIR"
}

setup_python() {
  if ! have uv; then
    warn "uv not found; skipping repo-local Python tooling setup"
    return 0
  fi

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
  ensure_repo
  prepare_host_environment
  ensure_fd_alias
  verify_bootstrap_requirements
  verify_emacs_version
  install_uv
  install_bash_lsp
  install_typescript_lsp
  install_ast_grep_cli
  install_zig_tooling
  install_bundled_sarasa_term_sc_fonts
  bootstrap_emacs_packages_and_compile
  setup_treesitter
  setup_python

  cat <<'EOF'

Done!

Next steps:
- Launch Emacs and enjoy.
- First launch should be near-ready: packages are pre-installed and all installed packages/config were byte-compiled during bootstrap.
- If `fd` command was missing but `fdfind` existed, we created a symlink in ~/.local/bin.
- For full feature set, verify commands exist: `ffmpegthumbnailer`, `pdftoppm`, `7zz`/`7z`, `mediainfo`, `exiftool`, `pandoc`, `ast-grep`/`sg`.

Tip: Re-run this installer any time; it is safe and idempotent.

EOF
}

main "$@"
