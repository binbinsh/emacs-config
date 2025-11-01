;; Tree-sitter: auto-install grammars and prefer *-ts-mode when available

(use-package treesit-auto
  :if (and (fboundp 'treesit-available-p) (treesit-available-p))
  :init
  ;; Prompt to install missing grammars when opening a file
  (setq treesit-auto-install 'prompt)
  

  ;; Prefer tree-sitter majors where possible
  (dolist (pair '((c-mode . c-ts-mode)
                  (c++-mode . c++-ts-mode)
                  (java-mode . java-ts-mode)
                  (js-mode . js-ts-mode)
                  (typescript-mode . typescript-ts-mode)
                  (python-mode . python-ts-mode)
                  (css-mode . css-ts-mode)
                  (conf-toml-mode . toml-ts-mode)
                  (go-mode . go-ts-mode)
                  (rust-mode . rust-ts-mode)
                  (bash-mode . bash-ts-mode)
                  (cmake-mode . cmake-ts-mode)))
    (add-to-list 'major-mode-remap-alist pair))

  ;; Ensure common extensions map to TS modes even without legacy modes
  (dolist (ext '(("\\.js\\'" . js-ts-mode)
                 ("\\.mjs\\'" . js-ts-mode)
                 ("\\.cjs\\'" . js-ts-mode)
                 ("\\.ts\\'" . typescript-ts-mode)
                 ("\\.tsx\\'" . tsx-ts-mode)
                 ("\\.json\\'" . json-ts-mode)
                 ("\\.css\\'" . css-ts-mode)
                 ("\\.toml\\'" . toml-ts-mode)
                 ("\\.go\\'" . go-ts-mode)
                 ("\\.rs\\'" . rust-ts-mode)
                 ("\\.cmake\\'" . cmake-ts-mode)
                 ("CMakeLists\\.txt\\'" . cmake-ts-mode)
                 ("\\.sh\\'" . bash-ts-mode)))
    (add-to-list 'auto-mode-alist ext))

  ;; Better imenu for TS majors when available
  (defun my/treesit-maybe-setup-imenu ()
    (when (fboundp 'treesit-simple-imenu-setup)
      (treesit-simple-imenu-setup)))
  (add-hook 'prog-mode-hook #'my/treesit-maybe-setup-imenu)
  :config
  (when (fboundp 'global-treesit-auto-mode)
    (global-treesit-auto-mode)))

(provide 'setup-treesit)


