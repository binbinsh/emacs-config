;; Keep startup fast; restore sane values after init
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(defvar my/file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Package setup happens in init.el; speed autoloads
(setq package-enable-at-startup nil)
;; Write quickstart cache under XDG cache (~/.cache/emacs/)
(let* ((xdg-cache (or (getenv "XDG_CACHE_HOME") (expand-file-name "~/.cache")))
       (cache-dir (file-name-as-directory (expand-file-name "emacs" xdg-cache))))
  (setq package-quickstart-file (expand-file-name "package-quickstart.el" cache-dir))
  (make-directory cache-dir t))
(setq package-quickstart t)

;; Reduce costly frame resizes during theme/UI setup
(setq frame-inhibit-implied-resize t)
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/file-name-handler-alist-old)
            (garbage-collect)))


