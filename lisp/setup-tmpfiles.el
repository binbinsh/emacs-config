(setq vc-make-backup-files t)

;; Base cache directory: ~/.cache/emacs/ (or $XDG_CACHE_HOME/emacs)
(defconst my-emacs-cache-directory
  (let ((xdg-cache (or (getenv "XDG_CACHE_HOME")
                       (expand-file-name "~/.cache"))))
    (file-name-as-directory (expand-file-name "emacs" xdg-cache)))
  "Directory where Emacs writes cached and temporary files.")

;; IMPORTANT: Early init timing
;; The following must be kept at the very top of this file and this file
;; must be required from early-init.el. They need to run as early as possible,
;; before any package initialization and before any native compilation happens:

;; Write package quickstart under XDG cache (~/.cache/emacs/)
(let ((cache-dir my-emacs-cache-directory))
  (setq package-quickstart-file (expand-file-name "package-quickstart.el" cache-dir))
  (make-directory cache-dir t))
(setq package-quickstart t)

;; Redirect native-comp eln cache under XDG cache (~/.cache/emacs/eln-cache)
(let ((eln-cache (file-name-as-directory (expand-file-name "eln-cache" my-emacs-cache-directory))))
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p)
             (fboundp 'startup-redirect-eln-cache))
    (startup-redirect-eln-cache eln-cache)))

;; ============================================================


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
                  "dirvish"
                  "svg-lib"
                  "lsp"))
  (make-directory (expand-file-name subdir my-emacs-cache-directory) t))

;; Redirect URL (eww) cache/config under XDG cache
(setq url-configuration-directory
      (expand-file-name "url/" my-emacs-cache-directory))
(setq url-cache-directory
      (expand-file-name "url/cache" my-emacs-cache-directory))

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

;; project.el known projects list file
(setq project-list-file
      (expand-file-name "projects" my-emacs-cache-directory))

(setq transient-history-file
      (expand-file-name "transient/history.el" my-emacs-cache-directory))
(setq transient-levels-file
      (expand-file-name "transient/levels.el" my-emacs-cache-directory))
(setq transient-values-file
      (expand-file-name "transient/values.el" my-emacs-cache-directory))

;; lsp-mode and dap-mode state files in cache dir
(setq lsp-session-file (expand-file-name ".lsp-session-v1" my-emacs-cache-directory))
(setq dap-breakpoints-file (expand-file-name ".dap-breakpoints" my-emacs-cache-directory))

;; Place lsp-mode server installs under XDG cache (~/.cache/emacs/lsp)
(setq lsp-server-install-dir (expand-file-name "lsp/" my-emacs-cache-directory))

;; svg-lib icons dir under XDG cache (~/.cache/emacs/svg-lib)
(let ((new-svg-cache (expand-file-name "svg-lib/" my-emacs-cache-directory)))
  (setq svg-lib-icons-dir new-svg-cache)
  (make-directory new-svg-cache t))

(provide 'setup-tmpfiles)
