;;; early-init.el --- Pre-GUI initialization -*- lexical-binding: t -*-
;;
;; This file runs before package.el and GUI frame creation.
;; Keep it minimal: GC tuning, UI chrome, package archives, cache paths.
;;
;; Structure:
;; 0. Terminal Compatibility
;; 1. GC and Startup Optimization

;; ============================================================================
;; 0. TERMINAL COMPATIBILITY
;; ============================================================================

;; Fix kitty terminal cursor position issues
(when (string= (getenv "TERM") "xterm-kitty")
  (setenv "TERM" "xterm-256color"))

;; ============================================================================
;; 1. GC AND STARTUP OPTIMIZATION
;; 2. Package Archives
;; 3. Cache Directory Setup
;; 4. UI Chrome (disable before frame creation)
;; 5. Startup Hooks

;; ============================================================================
;; 1. GC AND STARTUP OPTIMIZATION
;; ============================================================================

;; Maximize GC threshold during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Disable file-name-handler during startup (regex matching is expensive)
(defvar my/file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Reduce frame resize cost during theme/UI setup
(setq frame-inhibit-implied-resize t)

;; UTF-8 encoding
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; ============================================================================
;; 2. PACKAGE ARCHIVES
;; ============================================================================

;; Disable package.el at startup (use-package handles this in init.el)
(setq package-enable-at-startup nil)

;; China mirror for faster package downloads
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; ============================================================================
;; 3. CACHE DIRECTORY SETUP
;; ============================================================================

;; Base cache directory: ~/.cache/emacs/ (follows XDG spec)
(defconst my-emacs-cache-directory
  (let ((xdg-cache (or (getenv "XDG_CACHE_HOME")
                       (expand-file-name "~/.cache"))))
    (file-name-as-directory (expand-file-name "emacs" xdg-cache)))
  "Directory where Emacs writes cached and temporary files.")

;; Create cache subdirectories
(dolist (subdir '("auto-save" "auto-save-list" "backups" "bookmarks" "eshell"
                  "eln-cache" "locks" "tmp" "tramp" "url" "transient"
                  "dirvish" "svg-lib" "lsp"))
  (make-directory (expand-file-name subdir my-emacs-cache-directory) t))

;; Package quickstart for faster loading
(setq package-quickstart-file (expand-file-name "package-quickstart.el" my-emacs-cache-directory))
(setq package-quickstart t)

;; Redirect native-comp eln cache
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p)
           (fboundp 'startup-redirect-eln-cache))
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache" my-emacs-cache-directory)))

;; ============================================================================
;; 4. UI CHROME (DISABLE BEFORE FRAME CREATION)
;; ============================================================================

;; Set frame parameters before creation (faster than calling modes later)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable UI elements
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Keep menu bar on macOS (native integration feels better)
(unless (eq system-type 'darwin)
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

;; Default frame size (larger window without forcing fullscreen)
(push '(width . 150) default-frame-alist)
(push '(height . 75) default-frame-alist)

;; Disable vc-mode checking on file open (major source of slowness)
;; We use magit/diff-hl instead
(setq vc-handled-backends nil)

;; ============================================================================
;; 5. STARTUP HOOKS
;; ============================================================================

;; Restore sane GC values after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/file-name-handler-alist-old)
            (garbage-collect)))

;; Report startup time
(add-hook 'window-setup-hook
          (lambda ()
            (message "Emacs ready in %s with %d GCs."
                     (emacs-init-time) gcs-done)))

;;; early-init.el ends here
