(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-enable-math t
        markdown-fontify-code-blocks-natively t
        markdown-live-preview-use-content-type t)

  (defun my/markdown--prefer-in-emacs-browser ()
    "Prefer xwidget-webkit if available; otherwise fallback to EWW."
    (setq-local browse-url-browser-function
                (if (featurep 'xwidget-internal)
                    #'xwidget-webkit-browse-url
                  #'eww-browse-url)))

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


