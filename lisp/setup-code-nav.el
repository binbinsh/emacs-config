;; Navigation and syntax helpers across languages

;; Show current function in mode line
(which-function-mode 1)

;; Bracket depth coloring for Lisp-family and other languages
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Xref fallback to ripgrep-based dumb-jump when LSP/tags are unavailable
(use-package dumb-jump
  :init
  (setq dumb-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Optional symbol sidebar
(use-package imenu-list
  :commands (imenu-list-smart-toggle))

;; Imenu menu is built-in; Consult provides better UI (keys are in setup-keys.el)
;; Improve Imenu rescanning
(setq imenu-auto-rescan t
      imenu-use-popup-menu nil)

(provide 'setup-code-nav)


