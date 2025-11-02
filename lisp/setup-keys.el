;;; setup-keys.el --- C-c shortcuts (global and mode-specific)
;;
;; Overview
;; - Global and mode-specific actions use C-c + single keys.
;; - M-m leader is no longer used.
;;
;; Global shortcuts (C-c + key)
;; - m: Gmail (Notmuch UI)
;; - p: project files
;; - o: open file
;; - b: switch buffer
;; - /: ripgrep
;; - s: snippet select
;; - e: focus explorer
;; - v: toggle vterm panel
;; - t: Remote Dired (TRAMP)
;; - g: Magit status
;; - [: prev tab   ]: next tab
;; - n: new tab    x: close tab
;; - u: vundo (undo tree)
;; - y: show inline commit (blamer)
;; - l: toggle inline blame
;; - d: go to definition
;; - r: find references
;;
;; Mode-specific leaders (auto-selected per buffer)
;; - Dired/Dirvish: o open, c copy, r rename, + mkdir, d delete, p preview, 2 two-panes
;; - vterm: o open, h SSH, l Remote Dired, a AI suggest, r list tunnels
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
(global-set-key (kbd "C-c e") #'my/focus-explorer)
(global-set-key (kbd "C-c v") #'my/toggle-vterm)
(global-set-key (kbd "C-c g") #'fork-git-open-status)
(global-set-key (kbd "C-c s") #'my/snippet-select-smart)
(global-set-key (kbd "C-c /") #'consult-ripgrep)
(global-set-key (kbd "C-c b") #'consult-buffer)
(global-set-key (kbd "C-c p") #'project-find-file)
(global-set-key (kbd "C-c m") #'my/notmuch-open-gmail-ui)
(global-set-key (kbd "C-c o") #'find-file)
(global-set-key (kbd "C-c h") #'my/terminal-ssh-connect)
(global-set-key (kbd "C-c t") #'my/terminal-remote-dired)
(global-set-key (kbd "C-c u") #'vundo)
(global-set-key (kbd "C-c [") #'tab-previous)
(global-set-key (kbd "C-c ]") #'tab-next)
(global-set-key (kbd "C-c n") #'tab-new)
(global-set-key (kbd "C-c x") #'tab-close)
(global-set-key (kbd "C-c y") #'fork-git-show-inline-commit)
(global-set-key (kbd "C-c l") #'fork-git-inline-blame-toggle)

;; Symbols: fast function/class navigation
(global-set-key (kbd "C-c j") #'consult-imenu)
(global-set-key (kbd "C-c d") #'xref-find-definitions)
(global-set-key (kbd "C-c r") #'xref-find-references)

;; No leader defaults

;; Dired/Dirvish: C-c single keys inside file explorers
(with-eval-after-load 'dired
  (let ((map dired-mode-map))
    (define-key map (kbd "C-c e") #'my/focus-explorer)
    (define-key map (kbd "C-c o") #'dired-find-file)
    (define-key map (kbd "C-c c") #'dired-do-copy)
    (define-key map (kbd "C-c r") #'dired-do-rename)
    (define-key map (kbd "C-c +") #'dired-create-directory)
    (define-key map (kbd "C-c d") #'dired-do-delete)
    (define-key map (kbd "C-c p") #'tc/dired-toggle-preview)
    (define-key map (kbd "C-c )") #'dired-git-info-mode)
    (define-key map (kbd "C-c 2") #'tc/dired-two-panes)))

;; Dirvish: ensure the same explorer toggle works inside Dirvish buffers
(with-eval-after-load 'dirvish
  (define-key dirvish-mode-map (kbd "C-c e") #'my/focus-explorer)
  (when (boundp 'dirvish-directory-view-mode-map)
    (define-key dirvish-directory-view-mode-map (kbd "C-c e") #'my/focus-explorer)))

;; vterm: C-c inside terminal buffers
(with-eval-after-load 'vterm
  (let ((map vterm-mode-map))
    (define-key map (kbd "C-c s") #'my/snippet-select-smart)
    (define-key map (kbd "C-c o") #'my/terminal-open)
    (define-key map (kbd "C-c h") #'my/terminal-ssh-connect)
    (define-key map (kbd "C-c l") #'my/terminal-remote-dired)
    (define-key map (kbd "C-c c") #'my/terminal-ai-suggest)
    (define-key map (kbd "C-c r") #'my/terminal-tunnel-list)))

;; Magit: C-c inside Magit buffers
(with-eval-after-load 'magit
  (let ((map magit-mode-map))
    (define-key map (kbd "C-c g") #'fork-git-open-status)
    (define-key map (kbd "C-c b") #'fork-git-branch-graph)
    (define-key map (kbd "C-c h") #'fork-git-file-history)
    (define-key map (kbd "C-c c") #'fork-git-generate-commit-message)
    (define-key map (kbd "C-c l") #'fork-git-blame-toggle)
    (define-key map (kbd "C-c d") #'fork-git-magit-delta-toggle)))

;; Python: pytest under C-c t (LSP helpers are already bound globally)
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c t") #'python-pytest))

;; LSP: keep leader actions; also provide C-c helpers
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c r") #'xref-find-references)
  (define-key lsp-mode-map (kbd "C-c .") #'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c f") #'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c i") #'lsp-organize-imports))

;; Markdown: live preview
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c l") #'markdown-live-preview-mode))

;; Minibuffer: convenience abort
(define-key minibuffer-local-map (kbd "C-c m") #'abort-recursive-edit)

;; Global AI override – ensure C-c a always triggers AI
(defvar my/ai-override-map (make-sparse-keymap))
(define-key my/ai-override-map (kbd "C-c a") #'my/ai-execute)

(defvar my/ai--emulation-alist
  `((my/ai-override-mode . ,my/ai-override-map)))

(define-minor-mode my/ai-override-mode
  "Override C-c a globally with AI assistant."
  :global t)

(add-to-list 'emulation-mode-map-alists 'my/ai--emulation-alist)
(my/ai-override-mode 1)

;; Notmuch: Gmail-like bindings and AI helpers
(with-eval-after-load 'notmuch
  (let ((map notmuch-search-mode-map))
    ;; Archive and star in search view
    (define-key map (kbd "e") (lambda () (interactive) (notmuch-search-tag (list "-inbox"))))
    (define-key map (kbd "*") (lambda () (interactive) (notmuch-search-tag (list "+flagged"))))
    ;; Quick jumps
    (define-key map (kbd "g i") (lambda () (interactive) (notmuch-search "tag:inbox and not tag:trash and not tag:spam")))
    (define-key map (kbd "g a") (lambda () (interactive) (notmuch-search "*")))))

(with-eval-after-load 'notmuch-show
  (let ((map notmuch-show-mode-map))
    ;; Archive and star in show view
    (define-key map (kbd "e") (lambda () (interactive) (notmuch-show-tag (list "-inbox"))))
    (define-key map (kbd "*") (lambda () (interactive) (notmuch-show-tag (list "+flagged"))))))

;; AI helpers in search (single key)
(with-eval-after-load 'notmuch
  (define-key notmuch-search-mode-map (kbd "l") #'my/notmuch-ai-suggest-labels)
  (define-key notmuch-search-mode-map (kbd "s") #'my/notmuch-ai-summarize-thread)
  (define-key notmuch-search-mode-map (kbd "r") #'my/notmuch-ai-generate-reply))

;; AI helpers in WebKit view (single key)
(with-eval-after-load 'xwidget
  (when (boundp 'xwidget-webkit-mode-map)
    (define-key xwidget-webkit-mode-map (kbd "l") #'my/notmuch-ai-suggest-labels)
    (define-key xwidget-webkit-mode-map (kbd "s") #'my/notmuch-ai-summarize-thread)
    (define-key xwidget-webkit-mode-map (kbd "r") #'my/notmuch-ai-generate-reply)))

;; AI helpers in notmuch-show (single key), for fallback when not using WebKit
(with-eval-after-load 'notmuch-show
  (define-key notmuch-show-mode-map (kbd "l") #'my/notmuch-ai-suggest-labels)
  (define-key notmuch-show-mode-map (kbd "s") #'my/notmuch-ai-summarize-thread)
  (define-key notmuch-show-mode-map (kbd "r") #'my/notmuch-ai-generate-reply))

(provide 'setup-keys)

