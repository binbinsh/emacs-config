;; Large files & long lines handling (fast and safe)

(eval-and-compile (require 'so-long nil t))

;; Enable fast long-line handling globally; only affects problematic files
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode 1))

;; Install and initialize VLF for chunked viewing of huge files
(use-package vlf
  :init (require 'vlf-setup))

;; Auto-open with VLF without prompting; show only a non-blocking message
(setq vlf-application 'dont-ask)

(defcustom my/large-file-vlf-threshold (* 64 1024 1024)
  "Open files >= this many bytes via VLF."
  :type 'integer
  :group 'files)

;; Let VLF intercept based on the same threshold
(setq large-file-warning-threshold my/large-file-vlf-threshold)

(defun my/large-file--maybe-vlf ()
  "Reopen current file with VLF if it exceeds `my/large-file-vlf-threshold'."
  (when-let* ((file (buffer-file-name))
              (attrs (ignore-errors (file-attributes file 'string)))
              (size (and attrs (file-attribute-size attrs))))
    (when (and size (>= size my/large-file-vlf-threshold)
               (not (derived-mode-p 'vlf-mode)))
      (let ((pos (point)))
        (vlf file)
        (goto-char pos)))))

(add-hook 'find-file-hook #'my/large-file--maybe-vlf)

(defun my/large-file-setup ()
  "Lighten heavy features in very large/long-line buffers."
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (bound-and-true-p diff-hl-mode)
    (diff-hl-mode -1))
  (when (bound-and-true-p lsp-mode)
    (lsp-mode -1))
  (when (bound-and-true-p flymake-mode)
    (flymake-mode -1))
  (when (bound-and-true-p flycheck-mode)
    (flycheck-mode -1))
  (when (boundp 'treesit-font-lock-level)
    (setq-local treesit-font-lock-level 1))
  (setq-local jit-lock-defer-time 0.25
              redisplay-skip-fontification-on-input t
              fast-but-imprecise-scrolling t)

  ;; Adaptive VLF batch size (local vs remote)
  (when (boundp 'vlf-batch-size)
    (setq-local vlf-batch-size
                (if (file-remote-p (or (buffer-file-name) default-directory))
                    (* 2 1024 1024)   ; 2MB for remote TRAMP
                    (* 16 1024 1024)))) ; 16MB for local SSD/HDD

  ;; Reduce IO and memory footprint for huge buffers
  (setq-local make-backup-files nil
              auto-save-default nil
              save-place nil
              undo-limit (* 256 1024)))

(add-hook 'vlf-mode-hook #'my/large-file-setup)
(add-hook 'so-long-mode-hook #'my/large-file-setup)

;; Informative, non-blocking notice when VLF activates
(defun my/large-file--announce ()
  (run-at-time 0.2 nil
               (lambda ()
                 (message "Large file (VLF): batch=%s, %s. C-c w soft-wrap; M-x vlf-set-batch-size to tune."
                          (if (boundp 'vlf-batch-size) vlf-batch-size "?")
                          (if (file-remote-p (or (buffer-file-name) default-directory)) "remote" "local")))))

(add-hook 'vlf-mode-hook #'my/large-file--announce)

(defun my/large-file-toggle-soft-wrap ()
  "Toggle soft wrap (visual-line-mode) in large files on demand."
  (interactive)
  (visual-line-mode 'toggle))

(add-hook 'vlf-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c w") #'my/large-file-toggle-soft-wrap)))

(add-hook 'so-long-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c w") #'my/large-file-toggle-soft-wrap)))

(provide 'setup-large-files)

