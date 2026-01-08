;;; init.el --- Main initialization -*- lexical-binding: t -*-
;;
;; Minimal synchronous setup. All features load async via post-init.el.
;;
;; Structure:
;; 1. Package Bootstrap
;; 2. User Profile
;; 3. Cache Paths
;; 4. Essential UI (synchronous)
;; 5. Theme and Fonts
;; 6. Async Loading Trigger

;; ============================================================================
;; 1. PACKAGE BOOTSTRAP
;; ============================================================================

(require 'package)
(unless package--initialized (package-initialize))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Better subprocess I/O throughput (benefits LSP)
(setq read-process-output-max (* 3 1024 1024))

;; ~/.local/bin for uv and other tools
(let ((local-bin (expand-file-name "~/.local/bin")))
  (when (file-directory-p local-bin)
    (add-to-list 'exec-path local-bin)
    (setenv "PATH" (concat local-bin path-separator (getenv "PATH")))))

;; ============================================================================
;; 2. USER PROFILE
;; ============================================================================

(setq user-full-name "Binbin Shen")
(setq user-mail-address "bbs2021@sjtu.edu.cn")

;; Magit repository roots
(setq magit-repository-directories
      (list (cons (expand-file-name "~/Projects/") 2)))

;; Shell for terminal
(defvar my/terminal-shell
  (or (getenv "SHELL")
      (if (eq system-type 'darwin) "/bin/zsh" "/bin/bash")))

;; ============================================================================
;; 3. CACHE PATHS (my-emacs-cache-directory from early-init.el)
;; ============================================================================

(setq vc-make-backup-files t)
(setq temporary-file-directory (expand-file-name "tmp" my-emacs-cache-directory))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" my-emacs-cache-directory) t)))
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" my-emacs-cache-directory))
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups" my-emacs-cache-directory))))
(when (boundp 'lock-file-name-transforms)
  (setq lock-file-name-transforms
        `((".*" ,(expand-file-name "locks/" my-emacs-cache-directory) t))))
(setq nsm-settings-file (expand-file-name "network-security.data" my-emacs-cache-directory))
(setq eshell-directory-name (expand-file-name "eshell" my-emacs-cache-directory))
(setq tramp-auto-save-directory (expand-file-name "tramp" my-emacs-cache-directory))
(setq tramp-persistency-file-name (expand-file-name "tramp/tramp" my-emacs-cache-directory))
(setq recentf-save-file (expand-file-name "recentf" my-emacs-cache-directory))
(setq savehist-file (expand-file-name "savehist" my-emacs-cache-directory))
(setq save-place-file (expand-file-name "places" my-emacs-cache-directory))
(setq bookmark-default-file (expand-file-name "bookmarks/bookmarks" my-emacs-cache-directory))
(setq url-configuration-directory (expand-file-name "url/" my-emacs-cache-directory))
(setq url-cache-directory (expand-file-name "url/cache" my-emacs-cache-directory))
(setq url-history-file (expand-file-name "url/history" my-emacs-cache-directory))
(setq project-list-file (expand-file-name "projects" my-emacs-cache-directory))
(setq transient-history-file (expand-file-name "transient/history.el" my-emacs-cache-directory))
(setq transient-levels-file (expand-file-name "transient/levels.el" my-emacs-cache-directory))
(setq transient-values-file (expand-file-name "transient/values.el" my-emacs-cache-directory))
(setq dirvish-cache-dir (expand-file-name "dirvish" my-emacs-cache-directory))
(setq lsp-session-file (expand-file-name ".lsp-session-v1" my-emacs-cache-directory))
(setq dap-breakpoints-file (expand-file-name ".dap-breakpoints" my-emacs-cache-directory))
(setq lsp-server-install-dir (expand-file-name "lsp/" my-emacs-cache-directory))
(setq svg-lib-icons-dir (expand-file-name "svg-lib/" my-emacs-cache-directory))

;; Disable custom file (write to cache, never load)
(setq custom-file (expand-file-name "custom.el" my-emacs-cache-directory))

;; ============================================================================
;; 4. ESSENTIAL UI (SYNCHRONOUS)
;; ============================================================================

(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq frame-resize-pixelwise t)
(setq column-number-mode t)

;; Smooth scrolling
(pixel-scroll-precision-mode 1)
(setq scroll-margin 8 scroll-conservatively 101 auto-window-vscroll nil)

;; Visual feedback
(global-hl-line-mode 1)
(show-paren-mode 1)

;; History and state
(recentf-mode 1)
(save-place-mode 1)
(savehist-mode 1)

;; Line numbers
(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))
(add-hook 'text-mode-hook (lambda () (display-line-numbers-mode 1)))

;; macOS PATH sync
(use-package exec-path-from-shell
  :if (and (memq system-type '(darwin)) (display-graphic-p))
  :init (setq exec-path-from-shell-arguments '("-l"))
  :config (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "LANG" "LC_ALL" "SSH_AUTH_SOCK")))

;; macOS modifier keys (Option as Meta; keep right Option for symbols)
(when (eq system-type 'darwin)
  (when (boundp 'mac-option-modifier)
    (setq mac-option-modifier 'meta
          mac-right-option-modifier 'none))
  (when (boundp 'ns-option-modifier)
    (setq ns-option-modifier 'meta
          ns-right-option-modifier 'none)))

;; Clipboard integration
(use-package simpleclip :init (simpleclip-mode 1))

;; GC smoothing
(use-package gcmh
  :init
  (setq gcmh-idle-delay 1.0 gcmh-high-cons-threshold (* 16 1024 1024))
  (add-hook 'emacs-startup-hook #'gcmh-mode t))

;; ============================================================================
;; 5. THEME AND FONTS
;; ============================================================================

;; Load theme synchronously (user sees this immediately)
(use-package monokai-pro-theme
  :config
  (add-to-list 'custom-theme-load-path user-emacs-directory)
  (unless (custom-theme-p 'monokai-light)
    (load (expand-file-name "monokai-light-theme.el" user-emacs-directory) nil t))
  (load-theme 'monokai-light t))

;; Font setup
(require 'cl-lib)

(defun my/find-first-font (fonts)
  "Return first available font from FONTS list."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(defun my/apply-fonts (&optional frame)
  "Apply preferred fonts to FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (let* ((english (or (my/find-first-font '("JetBrainsMono Nerd Font Mono"
                                                "JetBrainsMono Nerd Font"
                                                "JetBrains Mono"
                                                "SF Mono"
                                                "Menlo"))
                          "JetBrains Mono"))
             (cjk (or (my/find-first-font '("PingFang SC" "Noto Sans CJK SC")) "Noto Sans CJK SC"))
             (emoji (my/find-first-font '("Apple Color Emoji" "Noto Color Emoji"))))
        (set-face-attribute 'default nil :font (font-spec :family english :size 15 :weight 'light))
        (dolist (charset '(han kana cjk-misc bopomofo))
          (set-fontset-font t charset (font-spec :name cjk)))
        (when emoji (set-fontset-font t 'symbol (font-spec :name emoji) nil 'prepend))
        ;; Powerline symbols (U+E0A0-U+E0D4) - use Nerd Font
        (set-fontset-font t '(#xe0a0 . #xe0d4) (font-spec :family english))
        ;; Private Use Area for additional icons
        (set-fontset-font t '(#xf000 . #xf8ff) (font-spec :family english))
        (setq nerd-icons-font-family "Symbols Nerd Font Mono")))))

(add-hook 'emacs-startup-hook #'my/apply-fonts)
(add-hook 'after-make-frame-functions #'my/apply-fonts)

;; ============================================================================
;; 6. DIRED ENHANCEMENT (immediate for emacs -nw .)
;; ============================================================================

;; Dired sorting: directories first
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)
(setq ls-lisp-dirs-first t)

;; Dirvish: modern dired UI - loaded early for `emacs -nw .` support
(use-package dirvish
  :demand t
  :init
  (require 'nerd-icons nil t)
  :custom
  (dirvish-attributes '(nerd-icons vc-state subtree-state collapse file-size file-time))
  (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format '(:left (sort omit symlink) :right (index)))
  (dirvish-use-header-line t)
  (dirvish-use-mode-line nil)
  (dirvish-hide-details t)
  (dirvish-hide-cursor t)
  (dirvish-window-fringe 8)
  :config
  (dirvish-override-dired-mode)
  (with-eval-after-load 'dirvish-subtree
    (setq dirvish-subtree-state-style 'nerd
          dirvish-subtree-always-show-state t))
  (with-eval-after-load 'dirvish-side
    (setq dirvish-side-width 32
          dirvish-side-attributes '(nerd-icons subtree-state)
          dirvish-side-header-line-format '(:left (project) :right (free-space)))))

;; ============================================================================
;; 7. ASYNC LOADING TRIGGER
;; ============================================================================

(defun my/load-post-init ()
  "Load post-init.el asynchronously."
  (let ((post-init (expand-file-name "post-init.el" user-emacs-directory)))
    (when (file-exists-p post-init)
      (load post-init nil t))))

;; Load after 0.1s idle - gives Emacs time to render frame first
(run-with-idle-timer 0.1 nil #'my/load-post-init)

;; Native compile in background
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p)
           (fboundp 'native-compile-async))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (ignore-errors
                (native-compile-async user-emacs-directory 'recursively)))))

;;; init.el ends here
