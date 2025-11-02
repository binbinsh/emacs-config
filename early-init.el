;; Keep startup fast; restore sane values after init
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(defvar my/file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Package setup happens in init.el; speed autoloads
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load tmp/cache setup as early as possible
(require 'setup-tmpfiles)

;; Reduce costly frame resizes during theme/UI setup
(setq frame-inhibit-implied-resize t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/file-name-handler-alist-old)
            (garbage-collect)))


