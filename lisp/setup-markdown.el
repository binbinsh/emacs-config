(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-enable-math t
        markdown-fontify-code-blocks-natively t
        markdown-live-preview-use-content-type t)

  ;; Use xwidget-webkit for markdown live preview when available (must return buffer)
  (defun my/markdown-preview-with-xwidget (file)
    "Open preview FILE using xwidget-webkit and return the preview buffer.
Falls back to the default EWW window function when xwidget is unavailable."
    (let* ((abs (expand-file-name file))
           (url (concat "file://" abs)))
      (if (and (display-graphic-p)
               (require 'xwidget nil t)
               (fboundp 'xwidget-webkit-browse-url))
          (progn
            ;; If we have an existing preview buffer, kill it to avoid confusion
            (when (and (buffer-live-p markdown-live-preview-buffer)
                       (with-current-buffer markdown-live-preview-buffer
                         (derived-mode-p 'xwidget-webkit-mode)))
              (kill-buffer markdown-live-preview-buffer))
            (xwidget-webkit-browse-url url)
            ;; Return the most recently used xwidget-webkit buffer
            (cl-loop for buf in (buffer-list)
                     when (with-current-buffer buf (derived-mode-p 'xwidget-webkit-mode))
                     return buf))
        ;; Fallback to markdown's default EWW implementation
        (when (fboundp 'markdown-live-preview-window-eww)
          (markdown-live-preview-window-eww abs)))))

  (setq markdown-live-preview-window-function #'my/markdown-preview-with-xwidget)

  (defun my/browse-url-xwidget-or-eww (url &rest args)
    "Open URL in xwidget-webkit if available; otherwise use EWW."
    (if (and (display-graphic-p)
             (require 'xwidget nil t)
             (fboundp 'xwidget-webkit-browse-url))
        (apply #'xwidget-webkit-browse-url url args)
      (apply #'eww-browse-url url args)))

  (defun my/markdown--prefer-in-emacs-browser ()
    "Prefer xwidget-webkit if available; otherwise fallback to EWW."
    (setq-local browse-url-browser-function #'my/browse-url-xwidget-or-eww)
    (setq-local browse-url-handlers '((".*" . my/browse-url-xwidget-or-eww))))

  (defun my/markdown-auto-preview ()
    "Auto-start live preview using an in-Emacs browser and split right."
    (my/markdown--prefer-in-emacs-browser)
    (unless (bound-and-true-p markdown-live-preview-mode)
      (markdown-live-preview-mode 1)))

  (add-hook 'markdown-mode-hook #'my/markdown-auto-preview)
  (add-hook 'gfm-mode-hook #'my/markdown-auto-preview))

(defun my/display-xwidget-right (buffer _action)
  (with-current-buffer buffer (derived-mode-p 'xwidget-webkit-mode)))

(defun my/display-eww-right (buffer _action)
  (with-current-buffer buffer (derived-mode-p 'eww-mode)))

(add-to-list 'display-buffer-alist
             '(my/display-xwidget-right
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . right) (window-width . 0.5)
               (window-parameters . ((no-delete-other-windows . t)))))

(add-to-list 'display-buffer-alist
             '(my/display-eww-right
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . right) (window-width . 0.5)
               (window-parameters . ((no-delete-other-windows . t)))))

(provide 'setup-markdown)


