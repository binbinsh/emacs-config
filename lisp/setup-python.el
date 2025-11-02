;; Python IDE via LSP (pyright + ruff)
(use-package lsp-mode
  :commands lsp-deferred)
(use-package lsp-ui :after lsp-mode)
(use-package lsp-pyright :after lsp-mode)
(with-eval-after-load 'lsp-mode
  (require 'lsp-pyright))
(add-hook 'python-mode-hook #'lsp-deferred)

;; LSP preferences
(with-eval-after-load 'lsp-mode
  (setq lsp-completion-provider :capf
        lsp-prefer-flymake t
        lsp-ui-doc-enable t
        lsp-ui-sideline-enable t))

;; Ruff LSP via uv run (format, lint, imports) – register if lsp-ruff not installed
(unless (require 'lsp-ruff nil 'noerror)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection (lambda () '("uv" "run" "ruff-lsp")))
    :activation-fn (lsp-activate-on "python")
    :add-on? t
    :server-id 'ruff_lsp)))

;; Format + organize imports on save
(defun my/python-lsp-format+imports ()
  (when (eq major-mode 'python-mode)
    (lsp-format-buffer)
    (lsp-organize-imports)))
(add-hook 'before-save-hook #'my/python-lsp-format+imports)

;; Use Corfu via CAPF; no company configuration needed

;; Debugging via dap-mode and debugpy
(use-package dap-mode
  :after lsp-mode
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (let* ((proj (ignore-errors (project-current)))
         (root (cond
                ((and proj (fboundp 'project-root)) (project-root proj))
                (proj (car (project-roots proj)))
                (t default-directory)))
         (venv-py (expand-file-name ".venv/bin/python" root)))
    (setq dap-python-executable (if (file-exists-p venv-py) venv-py "python"))))

;; Pytest via uv run
(use-package python-pytest
  :config
  (setq python-pytest-executable "uv run pytest"))

(provide 'setup-python)
