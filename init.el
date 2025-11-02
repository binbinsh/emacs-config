;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq use-package-compute-statistics t
      use-package-verbose t)

;; Bootstrap use-package
(require 'package)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Improve subprocess I/O throughput (benefits LSP and VCS tooling)
(setq read-process-output-max (* 3 1024 1024))

;; Set configs dir
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Ensure ~/.local/bin is available (uv installs here on macOS/Linux)
(let ((local-bin (expand-file-name "~/.local/bin")))
  (when (file-directory-p local-bin)
    (add-to-list 'exec-path local-bin)
    (setenv "PATH" (concat local-bin path-separator (getenv "PATH")))))

;; Personal profile (optional; safe if missing)
(require 'setup-profile nil t)

;; Disable Custom file creation and loading
(setq custom-file null-device)

;; macOS PATH/env sync (minimize shell startup cost)
(use-package exec-path-from-shell
  :if (and (memq system-type '(darwin)) (display-graphic-p))
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "LANG" "LC_ALL" "SSH_AUTH_SOCK")))

;; Tmp/cache files (ensure paths set before enabling modes)
(require 'setup-tmpfiles)

;; Setup GUI
(require 'setup-gui)

;; Setup Features
(require 'setup-terminal)
(require 'setup-dired)
(require 'setup-python)
(require 'setup-git)
(require 'setup-markdown)

;; Languages: LSP hooks and navigation helpers
(require 'setup-langs)

;; Gmail/Notmuch + LM Studio integration
(require 'setup-gmail nil t)

;; Snippets (load before keys to provide commands)
(require 'setup-snippets)

;; Context-aware Command leader keys
(require 'setup-keys)

;; Large files & long lines (VLF + so-long)
(require 'setup-large-files)

;; Clipboard
(use-package simpleclip
  :init (simpleclip-mode 1))

;; Smooth GC in idle using gcmh (no behavior changes)
(use-package gcmh
  :init
  (setq gcmh-idle-delay 1.0
        gcmh-high-cons-threshold (* 16 1024 1024))
  ;; Enable after early-init's emacs-startup-hook has restored GC settings
  (add-hook 'emacs-startup-hook #'gcmh-mode t))

;; Background native compilation of local config for future startups
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p)
           (fboundp 'native-compile-async))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (ignore-errors
                (native-compile-async (expand-file-name "lisp" user-emacs-directory) 'recursively)))))
