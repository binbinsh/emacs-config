;;; setup-keys.el --- C-c shortcuts (global and mode-specific)
;;
;; Overview
;; - Global and mode-specific actions use C-c + single keys.
;; - M-m leader is no longer used.
;;
;; Global shortcuts (C-c + key)
;; - m: M-x (command palette)
;; - p: project files
;; - o: open file
;; - b: switch buffer
;; - s: ripgrep
;; - e: toggle explorer
;; - v: toggle vterm panel
;; - t: terminal menu
;; - g: Magit status
;; - [: prev tab   ]: next tab
;; - n: new tab    x: close tab
;; - u: vundo (undo tree)
;; - y: show inline commit (blamer)
;; - l: toggle inline blame
;;
;; Mode-specific leaders (auto-selected per buffer)
;; - Dired/Dirvish: o open, c copy, r rename, + mkdir, d delete, p preview, 2 two-panes
;; - vterm: o open, h SSH, l terminal menu, a AI suggest, r list tunnels
;; - Magit: g status, b branch graph, h file history, a AI commit
;; - Python: a code action, r rename, f format, i organize imports, t pytest
;; - Markdown: l live preview
;; - Minibuffer: m abort (escape)
;;
;; Rationale
;; - Unify most shortcuts under a simple two-key ⌘ scheme to reduce complexity.
;; - Keep global muscle memory the same while allowing per-mode specializations.
;; - All bindings are centralized here for easy auditing and changes.
;;
;;; Code:

(require 'project)

;; Global Command leader map
;; No leader; use C-c everywhere
;; Direct shortcuts under C-c (user-reserved prefix)
(global-set-key (kbd "C-c e") #'my/toggle-explorer)
(global-set-key (kbd "C-c v") #'my/toggle-vterm)
(global-set-key (kbd "C-c g") #'fork-git-open-status)
(global-set-key (kbd "C-c s") #'consult-ripgrep)
(global-set-key (kbd "C-c b") #'consult-buffer)
(global-set-key (kbd "C-c p") #'project-find-file)
(global-set-key (kbd "C-c m") #'execute-extended-command)
(global-set-key (kbd "C-c o") #'find-file)
(global-set-key (kbd "C-c t") #'my/terminal-menu)
(global-set-key (kbd "C-c u") #'vundo)
(global-set-key (kbd "C-c [") #'tab-previous)
(global-set-key (kbd "C-c ]") #'tab-next)
(global-set-key (kbd "C-c n") #'tab-new)
(global-set-key (kbd "C-c x") #'tab-close)
(global-set-key (kbd "C-c y") #'fork-git-show-inline-commit)
(global-set-key (kbd "C-c l") #'fork-git-inline-blame-toggle)

;; No leader defaults

;; Dired/Dirvish: C-c single keys inside file explorers
(with-eval-after-load 'dired
  (let ((map dired-mode-map))
    (define-key map (kbd "C-c o") #'dired-find-file)
    (define-key map (kbd "C-c c") #'dired-do-copy)
    (define-key map (kbd "C-c r") #'dired-do-rename)
    (define-key map (kbd "C-c +") #'dired-create-directory)
    (define-key map (kbd "C-c d") #'dired-do-delete)
    (define-key map (kbd "C-c p") #'tc/dired-toggle-preview)
    (define-key map (kbd "C-c )") #'dired-git-info-mode)
    (define-key map (kbd "C-c 2") #'tc/dired-two-panes)))

;; vterm: C-c inside terminal buffers
(with-eval-after-load 'vterm
  (let ((map vterm-mode-map))
    (define-key map (kbd "C-c o") #'my/terminal-open)
    (define-key map (kbd "C-c h") #'my/terminal-ssh-connect)
    (define-key map (kbd "C-c l") #'my/terminal-menu)
    (define-key map (kbd "C-c a") #'my/terminal-ai-suggest)
    (define-key map (kbd "C-c r") #'my/terminal-tunnel-list)))

;; Magit: C-c inside Magit buffers
(with-eval-after-load 'magit
  (let ((map magit-mode-map))
    (define-key map (kbd "C-c g") #'fork-git-open-status)
    (define-key map (kbd "C-c b") #'fork-git-branch-graph)
    (define-key map (kbd "C-c h") #'fork-git-file-history)
    (define-key map (kbd "C-c a") #'fork-git-generate-commit-message)
    (define-key map (kbd "C-c l") #'fork-git-blame-toggle)
    (define-key map (kbd "C-c d") #'fork-git-magit-delta-toggle)))

;; Python: pytest under C-c t (LSP helpers are already bound globally)
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c t") #'python-pytest))

;; LSP: keep leader actions; also provide C-c helpers
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c a") #'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c f") #'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c i") #'lsp-organize-imports))

;; Markdown: live preview
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c l") #'markdown-live-preview-mode))

;; Minibuffer: convenience abort
(define-key minibuffer-local-map (kbd "C-c m") #'abort-recursive-edit)

;; No leader override system

(provide 'setup-keys)

