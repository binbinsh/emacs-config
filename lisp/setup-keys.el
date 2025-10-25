;;; setup-keys.el --- Context-aware Command (⌘) leader with two-key combos
;;
;; Overview
;; - macOS: "s-" means Command (⌘). Linux/Windows: "s-" means Super/Windows.
;; - This file defines a single global leader: press ⌘-; (s-;) then a key.
;; - The leader is context-aware: in certain modes the second key set changes
;;   (Dired/Dirvish, vterm, Magit, Python, Markdown, minibuffer).
;;
;; Global leader (press s-; then key)
;; - m: Command palette (M-x)
;; - p: Quick Open (project files)
;; - f: Open file
;; - b: Switch buffer
;; - s: Save buffer
;; - e: Toggle Explorer (Neotree)
;; - v: Toggle Terminal (vterm panel)
;; - t: Terminal menu (open/hosts/tunnels)
;; - g: Git status (Magit)
;; - [: Previous tab   ]: Next tab
;; - n: New tab        x: Close tab
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
(define-prefix-command 'my/cmd-prefix)
(global-set-key (kbd "s-;") 'my/cmd-prefix)
;; Single-key convenience
(global-set-key (kbd "s-b") #'my/toggle-explorer)
(global-set-key (kbd "s-t") #'tab-new)
(global-set-key (kbd "s-w") #'tab-close)
(global-set-key (kbd "s-`") #'my/toggle-vterm)
(global-set-key (kbd "s-/") #'completion-at-point)
(global-set-key (kbd "s-?") #'consult-man)

;; Global defaults under Command leader
(define-key my/cmd-prefix (kbd "m") #'execute-extended-command)
(define-key my/cmd-prefix (kbd "p") #'project-find-file)
(define-key my/cmd-prefix (kbd "f") #'find-file)
(define-key my/cmd-prefix (kbd "b") #'consult-buffer)
(define-key my/cmd-prefix (kbd "s") #'save-buffer)
(define-key my/cmd-prefix (kbd "e") #'my/toggle-explorer)
(define-key my/cmd-prefix (kbd "v") #'my/toggle-vterm)
(define-key my/cmd-prefix (kbd "t") #'my/terminal-menu)
(define-key my/cmd-prefix (kbd "g") #'fork-git-open-status)
(define-key my/cmd-prefix (kbd "[") #'tab-previous)
(define-key my/cmd-prefix (kbd "]") #'tab-next)
(define-key my/cmd-prefix (kbd "n") #'tab-new)
(define-key my/cmd-prefix (kbd "x") #'tab-close)
(define-key my/cmd-prefix (kbd "/") #'consult-ripgrep)
;; Undo tree visualizer
(define-key my/cmd-prefix (kbd "u") #'vundo)

;; Inline blame (global leader)
(define-key my/cmd-prefix (kbd "i") #'fork-git-show-inline-commit)
(define-key my/cmd-prefix (kbd "I") #'fork-git-inline-blame-toggle)

;; Helper to build an override map that only rebinds the leader to a mode map
(defun my/cmd--override-map-for (leader-map)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-;") leader-map)
    map))

;; Dired/Dirvish specific leader
(define-prefix-command 'my/cmd-dired)
(with-eval-after-load 'dired
  (define-key my/cmd-dired (kbd "o") #'dired-find-file)
  (define-key my/cmd-dired (kbd "c") #'dired-do-copy)
  (define-key my/cmd-dired (kbd "r") #'dired-do-rename)
  (define-key my/cmd-dired (kbd "+") #'dired-create-directory)
  (define-key my/cmd-dired (kbd "d") #'dired-do-delete)
  (define-key my/cmd-dired (kbd "p") #'tc/dired-toggle-preview)
  (define-key my/cmd-dired (kbd ")") #'dired-git-info-mode)
  (define-key my/cmd-dired (kbd "2") #'tc/dired-two-panes))
(defvar my/cmd-dired-override (my/cmd--override-map-for 'my/cmd-dired))

;; vterm specific leader
(define-prefix-command 'my/cmd-vterm)
(with-eval-after-load 'vterm
  (define-key my/cmd-vterm (kbd "o") #'my/terminal-open)
  (define-key my/cmd-vterm (kbd "h") #'my/terminal-ssh-connect)
  (define-key my/cmd-vterm (kbd "l") #'my/terminal-menu)
  (define-key my/cmd-vterm (kbd "a") #'my/terminal-ai-suggest)
  (define-key my/cmd-vterm (kbd "r") #'my/terminal-tunnel-list))
(defvar my/cmd-vterm-override (my/cmd--override-map-for 'my/cmd-vterm))

;; Magit specific leader (lightweight)
(define-prefix-command 'my/cmd-magit)
(with-eval-after-load 'magit
  (define-key my/cmd-magit (kbd "g") #'fork-git-open-status)
  (define-key my/cmd-magit (kbd "b") #'fork-git-branch-graph)
  (define-key my/cmd-magit (kbd "h") #'fork-git-file-history)
  (define-key my/cmd-magit (kbd "a") #'fork-git-generate-commit-message)
  ;; Magit: blame and delta toggles
  (define-key my/cmd-magit (kbd "l") #'fork-git-blame-toggle)
  (define-key my/cmd-magit (kbd "d") #'fork-git-magit-delta-toggle))
(defvar my/cmd-magit-override (my/cmd--override-map-for 'my/cmd-magit))

;; Python specific leader
(define-prefix-command 'my/cmd-python)
(with-eval-after-load 'lsp-mode
  (define-key my/cmd-python (kbd "a") #'lsp-execute-code-action)
  (define-key my/cmd-python (kbd "r") #'lsp-rename)
  (define-key my/cmd-python (kbd "f") #'lsp-format-buffer)
  (define-key my/cmd-python (kbd "i") #'lsp-organize-imports))
(with-eval-after-load 'python-pytest
  (define-key my/cmd-python (kbd "t") #'python-pytest))
(defvar my/cmd-python-override (my/cmd--override-map-for 'my/cmd-python))

;; consult-lsp: Command bindings in any LSP buffer (no C-c prefix)
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'consult-lsp
    (define-key lsp-mode-map (kbd "s-?") #'consult-lsp-symbols)       ;; workspace symbols
    (define-key lsp-mode-map (kbd "s-s") #'consult-lsp-file-symbols)  ;; file symbols
    (define-key lsp-mode-map (kbd "s-d") #'consult-lsp-diagnostics))  ;; diagnostics
  (define-key lsp-mode-map (kbd "s-r") #'lsp-find-references))         ;; references

;; Markdown specific leader
(define-prefix-command 'my/cmd-markdown)
(with-eval-after-load 'markdown-mode
  (define-key my/cmd-markdown (kbd "l") #'markdown-live-preview-mode))
(defvar my/cmd-markdown-override (my/cmd--override-map-for 'my/cmd-markdown))

;; Minibuffer specific leader (focus search/completion)
(define-prefix-command 'my/cmd-minibuffer)
(define-key my/cmd-minibuffer (kbd "m") #'abort-recursive-edit)
(defvar my/cmd-minibuffer-override (my/cmd--override-map-for 'my/cmd-minibuffer))

;; Emulation map: dynamically override leader by context
(defvar-local my/cmd--current-override nil)
(defvar my/cmd-emulation-alist '((my/cmd-local-mode . my/cmd--current-override)))
(add-to-list 'emulation-mode-map-alists 'my/cmd-emulation-alist)

(define-minor-mode my/cmd-local-mode
  "Context-aware Command leader override."
  :global t :init-value t)

(defun my/cmd--set-override (map)
  (setq-local my/cmd--current-override map))

;; Hooks to select appropriate override per buffer
(add-hook 'dired-mode-hook (lambda () (my/cmd--set-override my/cmd-dired-override)))
(add-hook 'vterm-mode-hook (lambda () (my/cmd--set-override my/cmd-vterm-override)))
(with-eval-after-load 'magit
  (dolist (h '(magit-status-mode-hook magit-log-mode-hook git-commit-mode-hook))
    (add-hook h (lambda () (my/cmd--set-override my/cmd-magit-override)))))
(add-hook 'python-mode-hook (lambda () (my/cmd--set-override my/cmd-python-override)))
(add-hook 'markdown-mode-hook (lambda () (my/cmd--set-override my/cmd-markdown-override)))
(add-hook 'minibuffer-setup-hook (lambda () (my/cmd--set-override my/cmd-minibuffer-override)))
(add-hook 'minibuffer-exit-hook (lambda () (my/cmd--set-override nil)))

(provide 'setup-keys)

