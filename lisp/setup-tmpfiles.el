(setq vc-make-backup-files t)

;; Base cache directory: ~/.cache/emacs/ (or $XDG_CACHE_HOME/emacs)
(defconst my-emacs-cache-directory
  (let ((xdg-cache (or (getenv "XDG_CACHE_HOME")
                       (expand-file-name "~/.cache"))))
    (file-name-as-directory (expand-file-name "emacs" xdg-cache)))
  "Directory where Emacs writes cached and temporary files.")

;; Dirvish cache directory under XDG cache (~/.cache/emacs/dirvish)
(setq dirvish-cache-dir (expand-file-name "dirvish" my-emacs-cache-directory))

;; Ensure cache subdirectories exist
(dolist (subdir '("auto-save"
                  "auto-save-list"
                  "backups"
                  "bookmarks"
                  "eshell"
                  "eln-cache"
                  "locks"
                  "tmp"
                  "tramp"
                  "url"
                  "transient"
                  "dirvish"))
  (make-directory (expand-file-name subdir my-emacs-cache-directory) t))

;; Temporary files
(setq temporary-file-directory
      (expand-file-name "tmp" my-emacs-cache-directory))

;; Auto-save files and list
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" my-emacs-cache-directory) t)))
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" my-emacs-cache-directory))

;; Backups
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups" my-emacs-cache-directory))))

;; Lock files (Emacs 29+)
(when (boundp 'lock-file-name-transforms)
  (setq lock-file-name-transforms
        `((".*" ,(expand-file-name "locks/" my-emacs-cache-directory) t))))

;; Network Security Manager state
(setq nsm-settings-file
      (expand-file-name "network-security.data" my-emacs-cache-directory))

;; Eshell data
(setq eshell-directory-name
      (expand-file-name "eshell" my-emacs-cache-directory))

;; TRAMP cache/state
(setq tramp-auto-save-directory
      (expand-file-name "tramp" my-emacs-cache-directory))
(setq tramp-persistency-file-name
      (expand-file-name "tramp/tramp" my-emacs-cache-directory))

;; History/state files commonly treated as cache
(setq recentf-save-file
      (expand-file-name "recentf" my-emacs-cache-directory))
(setq savehist-file
      (expand-file-name "savehist" my-emacs-cache-directory))
(setq save-place-file
      (expand-file-name "places" my-emacs-cache-directory))
(setq bookmark-default-file
      (expand-file-name "bookmarks/bookmarks" my-emacs-cache-directory))
(setq url-history-file
      (expand-file-name "url/history" my-emacs-cache-directory))

(setq transient-history-file
      (expand-file-name "transient/history.el" my-emacs-cache-directory))
(setq transient-levels-file
      (expand-file-name "transient/levels.el" my-emacs-cache-directory))
(setq transient-values-file
      (expand-file-name "transient/values.el" my-emacs-cache-directory))

;; lsp-mode and dap-mode state files in cache dir
(setq lsp-session-file (expand-file-name ".lsp-session-v1" my-emacs-cache-directory))
(setq dap-breakpoints-file (expand-file-name ".dap-breakpoints" my-emacs-cache-directory))

(provide 'setup-tmpfiles)
