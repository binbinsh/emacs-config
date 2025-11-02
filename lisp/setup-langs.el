;; Multi-language modes, syntax coloring, and LSP hooks

(use-package lsp-mode
  :commands lsp-deferred)

;; Tree-sitter: auto-install grammars and prefer *-ts-mode when available
(use-package treesit-auto
  :if (and (fboundp 'treesit-available-p) (treesit-available-p))
  :init
  ;; Prompt to install missing grammars when opening a file
  (setq treesit-auto-install 'prompt)

  ;; Don't warn when no LSP clients are found
  (setq lsp-warn-no-matched-clients nil)
  
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

;; HTML/JSX/TSX and templates
(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :mode ("\\.xhtml\\'" . web-mode)
  :mode ("\\.shtml\\'" . web-mode)
  :mode ("\\.tsx\\'" . web-mode)
  :mode ("\\.jsx\\'" . web-mode)
  :init
  (add-hook 'web-mode-hook #'lsp-deferred))

;; R language
(use-package ess
  :mode ("\\.[rR]\\'" . R-mode)
  :init
  (add-hook 'ess-r-mode-hook #'lsp-deferred))

;; Swift
(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :init
  (add-hook 'swift-mode-hook #'lsp-deferred))

;; Go (ts-mode preferred; go-mode as fallback)
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :init
  (add-hook 'go-mode-hook #'lsp-deferred))
(with-eval-after-load 'go-ts-mode
  (add-hook 'go-ts-mode-hook #'lsp-deferred))

;; Rust (ts-mode preferred)
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (add-hook 'rust-mode-hook #'lsp-deferred))
(with-eval-after-load 'rust-ts-mode
  (add-hook 'rust-ts-mode-hook #'lsp-deferred))

;; Lua
(with-eval-after-load 'lua-mode
  (add-hook 'lua-mode-hook #'lsp-deferred))

;; Java
(add-hook 'java-mode-hook #'lsp-deferred)
(with-eval-after-load 'java-ts-mode
  (add-hook 'java-ts-mode-hook #'lsp-deferred))

;; JavaScript / TypeScript
(add-hook 'js-mode-hook #'lsp-deferred)
(with-eval-after-load 'js-ts-mode
  (add-hook 'js-ts-mode-hook #'lsp-deferred))
(with-eval-after-load 'typescript-ts-mode
  (add-hook 'typescript-ts-mode-hook #'lsp-deferred))
(with-eval-after-load 'tsx-ts-mode
  (add-hook 'tsx-ts-mode-hook #'lsp-deferred))

;; JSON
(with-eval-after-load 'json-ts-mode
  (add-hook 'json-ts-mode-hook #'lsp-deferred))

;; HTML (built-in mhtml mode) and CSS
(add-hook 'mhtml-mode-hook #'lsp-deferred)
(add-hook 'css-mode-hook #'lsp-deferred)
(with-eval-after-load 'css-ts-mode
  (add-hook 'css-ts-mode-hook #'lsp-deferred))

;; C / C++ / Objective-C
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)
(add-hook 'objc-mode-hook #'lsp-deferred)
(with-eval-after-load 'c-ts-mode
  (add-hook 'c-ts-mode-hook #'lsp-deferred))
(with-eval-after-load 'c++-ts-mode
  (add-hook 'c++-ts-mode-hook #'lsp-deferred))

;; Perl
(add-hook 'perl-mode-hook #'lsp-deferred)

;; Bash / Shell
(add-hook 'sh-mode-hook #'lsp-deferred)
(with-eval-after-load 'bash-ts-mode
  (add-hook 'bash-ts-mode-hook #'lsp-deferred))

;; TOML / YAML / CMake
(with-eval-after-load 'toml-ts-mode
  (add-hook 'toml-ts-mode-hook #'lsp-deferred))
(with-eval-after-load 'yaml-mode
  (add-hook 'yaml-mode-hook #'lsp-deferred))
(add-hook 'cmake-mode-hook #'lsp-deferred)
(with-eval-after-load 'cmake-ts-mode
  (add-hook 'cmake-ts-mode-hook #'lsp-deferred))

;; Lisp family: Emacs Lisp, Common Lisp, Scheme, Clojure
;; Emacs Lisp: built-in xref/imenu provide solid navigation
(use-package clojure-mode
  :mode ("\\.clj[cs]?\\'" . clojure-mode)
  :init
  (add-hook 'clojure-mode-hook #'lsp-deferred))

(provide 'setup-langs)


