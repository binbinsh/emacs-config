;;; post-init.el --- Async feature loading -*- lexical-binding: t -*-
;;
;; All features load asynchronously via run-with-idle-timer.
;; This file is loaded by init.el after 0.1s idle.
;;
;; Structure:
;; 1.  Async Loading Infrastructure
;; 2.  UI Enhancements (modeline, tabs, which-key)
;; 3.  Completion (Vertico, Orderless, Marginalia, Consult, Embark, Corfu)
;; 4.  Help and Documentation
;; 5.  Dired and File Explorer
;; 6.  Terminal and TRAMP
;; 7.  Git Integration (Magit, Fork-like UX)
;; 8.  Languages and LSP
;; 9.  Python IDE
;; 10. Markdown
;; 11. LaTeX Workspace
;; 12. Code Navigation and Syntax
;; 13. Keybindings
;; 14. Utility Functions

;; ============================================================================
;; 1. ASYNC LOADING INFRASTRUCTURE
;; ============================================================================

(require 'project)
(require 'seq)
(require 'subr-x)
(require 'cl-lib)

(defvar my/post-init-delay 0.0
  "Incremental delay for staggered feature loading.")

(defmacro my/load-feature (name &rest body)
  "Load feature NAME with BODY after incremental idle delay."
  (declare (indent 1))
  `(progn
     (setq my/post-init-delay (+ my/post-init-delay 0.05))
     (run-with-idle-timer my/post-init-delay nil
                          (lambda ()
                            (condition-case err
                                (progn ,@body)
                              (error (message "Error loading %s: %s" ,name err)))))))

;; ============================================================================
;; 2. UI ENHANCEMENTS
;; ============================================================================

(my/load-feature "doom-modeline"
  (use-package doom-modeline
    :init
    (setq doom-modeline-icon t
          doom-modeline-height 26
          doom-modeline-buffer-file-name-style 'truncate-upto-project)
    :config
    (doom-modeline-mode 1)
    ;; Custom modeline with keycast support
    (doom-modeline-def-modeline 'my-doom-modeline
      '(bar window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs check))
    (add-hook 'doom-modeline-mode-hook
              (lambda () (doom-modeline-set-modeline 'my-doom-modeline t)))))

(my/load-feature "keycast"
  (use-package keycast
    :init
    (add-to-list 'global-mode-string '("" keycast-mode-line))
    :config
    (define-minor-mode keycast-mode
      "Show current command and key binding in mode line."
      :global t
      (if keycast-mode
          (add-hook 'pre-command-hook #'keycast--update t)
        (remove-hook 'pre-command-hook #'keycast--update)))
    (keycast-mode 1)))

(my/load-feature "tab-bar"
  ;; Show tab bar when more than 1 tab
  (setq tab-bar-show 1
        tab-bar-close-button-show t
        tab-bar-new-button-show t
        tab-bar-tab-hints t
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-add-tab)
        tab-bar-close-tab-select 'recent
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil)

  (tab-bar-mode 1)

  ;; Modern styling
  (with-eval-after-load 'tab-bar
    (let ((bg "#fafafa")
          (bg-inactive "#eeeeee")
          (fg "#272822")
          (fg-inactive "#75715e")
          (accent "#f92672")
          (border "#dddddd"))
      ;; Tab bar background
      (set-face-attribute 'tab-bar nil
                          :background bg
                          :foreground fg
                          :box `(:line-width 4 :color ,bg :style nil)
                          :height 140)
      ;; Active tab
      (set-face-attribute 'tab-bar-tab nil
                          :background bg
                          :foreground fg
                          :weight 'semibold
                          :box `(:line-width 4 :color ,bg :style nil)
                          :underline `(:color ,accent :position t))
      ;; Inactive tabs
      (set-face-attribute 'tab-bar-tab-inactive nil
                          :background bg-inactive
                          :foreground fg-inactive
                          :weight 'normal
                          :box `(:line-width 4 :color ,bg-inactive :style nil)))))

(my/load-feature "fill-column"
  (setq-default display-fill-column-indicator-column 120)
  (global-display-fill-column-indicator-mode 1))

(my/load-feature "window-divider"
  (window-divider-mode 1)
  (setq window-divider-default-places 'right-only
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1))

(my/load-feature "which-key"
  (use-package which-key :config (which-key-mode 1))
  (use-package which-key-posframe
    :if (display-graphic-p)
    :after which-key
    :config (which-key-posframe-mode 1)))

(my/load-feature "transient-posframe"
  (unless (boundp 'transient-minimal-frame-width)
    (defvar transient-minimal-frame-width 80))
  (use-package transient-posframe
    :if (display-graphic-p)
    :after transient
    :config (transient-posframe-mode 1)))

;; ============================================================================
;; 3. COMPLETION: VERTICO + ORDERLESS + MARGINALIA + CONSULT + EMBARK + CORFU
;; ============================================================================

(my/load-feature "vertico"
  (use-package vertico
    :init
    (vertico-mode 1)
    (setq vertico-cycle t))
  (use-package vertico-posframe
    :if (display-graphic-p)
    :after vertico
    :config
    (setq vertico-posframe-width 120)
    (vertico-posframe-mode 1)))

(my/load-feature "orderless"
  (use-package orderless
    :custom
    (completion-styles '(orderless))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles basic partial-completion))))))

(my/load-feature "marginalia"
  (use-package marginalia :init (marginalia-mode 1)))

(my/load-feature "consult"
  (use-package consult
    :init
    (setq register-preview-delay 0.2
          register-preview-function #'consult-register-format
          xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
    (advice-add #'register-preview :override #'consult-register-window))
  (use-package consult-lsp
    :after (consult lsp-mode)
    :config
    (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)))

(my/load-feature "embark"
  (use-package embark
    :bind (("C-." . embark-act) ("C-;" . embark-dwim))
    :init (setq prefix-help-command #'embark-prefix-help-command))
  (use-package embark-consult
    :after (embark consult)
    :hook (embark-collect-mode . consult-preview-at-point-mode)))

(my/load-feature "corfu"
  (use-package corfu
    :init
    (setq corfu-auto t
          corfu-auto-delay 0.08
          corfu-auto-prefix 1
          corfu-quit-no-match 'separator
          corfu-preselect 'prompt
          corfu-scroll-margin 3)
    (global-corfu-mode 1))
  (with-eval-after-load 'corfu
    (when (require 'corfu-popupinfo nil t)
      (corfu-popupinfo-mode 1)
      (setq corfu-popupinfo-delay 0.15 corfu-popupinfo-hide nil)))
  (use-package cape
    :init
    (add-hook 'completion-at-point-functions #'cape-file)
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-keyword))
  (use-package kind-icon
    :after corfu
    :init (setq kind-icon-default-face 'corfu-default)
    :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

;; ============================================================================
;; 4. HELP AND DOCUMENTATION
;; ============================================================================

(my/load-feature "helpful"
  (use-package helpful
    :init
    (define-key help-map (kbd "f") #'helpful-callable)
    (define-key help-map (kbd "v") #'helpful-variable)
    (define-key help-map (kbd "k") #'helpful-key)
    (global-set-key (kbd "C-h F") #'helpful-function)
    (global-set-key (kbd "C-h C") #'helpful-command)))

(my/load-feature "eldoc-box"
  (use-package eldoc-box
    :if (display-graphic-p)
    :hook ((prog-mode . eldoc-box-hover-mode))
    :init
    (setq eldoc-box-max-pixel-width 600
          eldoc-box-max-pixel-height 400)))

;; ============================================================================
;; 5. DIRED AND FILE EXPLORER
;; ============================================================================

(my/load-feature "dired"
  ;; Use ls-lisp for cross-platform compatibility
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp)
  (setq ls-lisp-dirs-first t)
  (require 'dired)

  ;; Dired usability
  (setq dired-dwim-target t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-listing-switches "-alh"
        global-auto-revert-non-file-buffers t)
  (add-hook 'dired-mode-hook #'auto-revert-mode)

  ;; Visuals
  (add-hook 'dired-mode-hook (lambda ()
    (when (fboundp 'dired-hide-details-mode) (dired-hide-details-mode 1))
    (hl-line-mode 1)))

  ;; Dirvish: modern dired UI
  (use-package dirvish
    :defer t
    :init (require 'nerd-icons nil t)
    :config
    (dirvish-override-dired-mode)
    (setq dirvish-attributes (list (if (display-graphic-p) 'vscode-icon 'nerd-icons)
                                   'file-size 'file-time)))

  (use-package diredfl :hook (dired-mode . diredfl-mode))
  (use-package dired-git-info :defer t :commands dired-git-info-mode)
  (use-package peep-dired :defer t :commands peep-dired)

  ;; Quick preview toggle
  (defun tc/dired-toggle-preview ()
    "Toggle quick preview in Dirvish or Dired."
    (interactive)
    (cond
     ((and (boundp 'dirvish-mode) dirvish-mode (fboundp 'dirvish-peek-mode))
      (if (bound-and-true-p dirvish-peek-mode)
          (dirvish-peek-mode -1)
        (dirvish-peek-mode 1)))
     ((derived-mode-p 'dired-mode)
      (when (require 'peep-dired nil t)
        (if (bound-and-true-p peep-dired-mode)
            (peep-dired -1)
          (peep-dired 1))))))

  ;; Two-pane helper
  (defun tc/dired-two-panes ()
    "Open a two-pane Dirvish/Dired session."
    (interactive)
    (delete-other-windows)
    (if (fboundp 'dirvish)
        (progn
          (dirvish default-directory)
          (split-window-right)
          (other-window 1)
          (dirvish default-directory)
          (other-window 1))
      (progn
        (dired default-directory)
        (split-window-right)
        (other-window 1)
        (dired default-directory)
        (other-window 1))))

  ;; Keybindings
  (with-eval-after-load 'dired
    (let ((map dired-mode-map))
      (define-key map (kbd "V") #'tc/dired-toggle-preview)
      (define-key map (kbd ")") #'dired-git-info-mode)
      (define-key map (kbd "RET") #'dired-find-file)
      (define-key map (kbd "C") #'dired-do-copy)
      (define-key map (kbd "R") #'dired-do-rename)
      (define-key map (kbd "+") #'dired-create-directory)
      (define-key map (kbd "D") #'dired-do-delete)
      (define-key map (kbd "TAB") #'other-window))))

(my/load-feature "dirvish-side"
  (use-package vscode-icon :if (display-graphic-p))
  (use-package dirvish
    :after project
    :config
    (dirvish-override-dired-mode)
    (when (display-graphic-p)
      (setq dirvish-attributes '(vscode-icon file-size file-time)
            dirvish-side-attributes '(vscode-icon file-size))))

  (setq dirvish-side-display-alist '((side . left) (slot . -1))
        dirvish-side-width 40
        dirvish-side-window-parameters '((no-delete-other-windows . t)
                                         (no-other-window . t)))

  (defun my/dirvish-project-root ()
    "Open dirvish at project root or current directory."
    (let* ((project (ignore-errors (project-current)))
           (root (if project (project-root project) default-directory)))
      (let ((blocked (and (fboundp 'dirvish-curr)
                          (when-let* ((dv (dirvish-curr))) (dv-curr-layout dv)))))
        (if blocked
            (with-temp-buffer (dirvish-side root))
          (dirvish-side root)))))

  (defun my/toggle-explorer ()
    "Toggle explorer like VSCode sidebar."
    (interactive)
    (let ((w (and (fboundp 'dirvish-side--session-visible-p)
                  (dirvish-side--session-visible-p))))
      (if (and (windowp w) (window-live-p w))
          (with-selected-window w (dirvish-quit))
        (my/dirvish-project-root))))

  (defun my/dirvish-side-window ()
    "Return the Dirvish side window if visible."
    (catch 'win
      (dolist (w (window-list))
        (with-current-buffer (window-buffer w)
          (when (and (derived-mode-p 'dirvish-directory-view-mode)
                     (eq (window-parameter w 'no-other-window) t))
            (throw 'win w))))
      nil))

  (defun my/focus-explorer ()
    "Focus the explorer sidebar."
    (interactive)
    (let ((side (my/dirvish-side-window)))
      (cond
       ((and (windowp side) (window-live-p side))
        (select-window side))
       (t
        (ignore-errors (my/dirvish-project-root))
        (when-let* ((w (my/dirvish-side-window)))
          (when (window-live-p w)
            (select-window w))))))))

;; ============================================================================
;; 6. TERMINAL AND TRAMP
;; ============================================================================

(my/load-feature "terminal"
  ;; vterm terminal
  (use-package vterm
    :commands (vterm vterm-mode)
    :init
    (setq vterm-always-compile-module t)
    :config
    (setq vterm-shell my/terminal-shell
          vterm-max-scrollback 10240
          vterm-kill-buffer-on-exit t
          vterm-copy-mode-remove-fake-newlines t))

  ;; Disable visual clutter in terminal
  (add-hook 'vterm-mode-hook
            (lambda ()
              (display-fill-column-indicator-mode -1)
              (display-line-numbers-mode -1)
              (setq-local global-hl-line-mode nil)))

  ;; Terminal buffer naming
  (defvar-local my/terminal--static-name nil)

  (defun my/terminal--buffer-name (&optional directory)
    "Return canonical vterm buffer name for DIRECTORY."
    (let* ((raw (or directory default-directory "~"))
           (remote (file-remote-p raw))
           (path nil) (host nil))
      (if remote
          (let* ((vec (ignore-errors (tramp-dissect-file-name raw)))
                 (local (or (and vec (tramp-file-name-localname vec)) raw))
                 (remote-host (and vec (tramp-file-name-host vec))))
            (setq path (abbreviate-file-name (directory-file-name local)))
            (setq host (or (car (split-string (or remote-host "") "\\.")) "localhost")))
        (setq path (abbreviate-file-name (directory-file-name raw)))
        (setq host (car (split-string (or (system-name) "localhost") "\\."))))
      (format "vterm: %s@%s" (or path "~") (or host "localhost"))))

  (defun my/toggle-vterm ()
    "Toggle a bottom vterm panel (30% height)."
    (interactive)
    (let* ((name "vterm")
           (buf (get-buffer name))
           (win (and buf (get-buffer-window buf))))
      (if (and buf (window-live-p win))
          (delete-window win)
        (let* ((target-lines (floor (* 0.3 (frame-height))))
               (new-win (split-window (selected-window) (- target-lines) 'below)))
          (select-window new-win)
          (if (buffer-live-p buf)
              (switch-to-buffer buf)
            (vterm)
            (rename-buffer name t))))))

  (defun my/vterm-clipboard-yank ()
    "Paste system clipboard into vterm."
    (interactive)
    (let* ((text (cond
                  ((fboundp 'simpleclip-get-contents) (simpleclip-get-contents))
                  ((fboundp 'gui-get-selection) (gui-get-selection 'CLIPBOARD))
                  (t nil))))
      (when (and text (not (string-empty-p text)))
        (if (derived-mode-p 'vterm-mode)
            (vterm-send-string text)
          (insert text))))))

(my/load-feature "tramp"
  (require 'tramp)
  (setq tramp-default-method "ssh")

  ;; SSH connection sharing
  (setq tramp-use-ssh-controlmaster-options t
        tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath=~/.ssh/tramp.%%C -o ControlPersist=600")

  ;; Cache settings
  (setq remote-file-name-inhibit-cache nil
        tramp-completion-reread-directory-timeout nil
        tramp-cache-read-persistent-data t)

  ;; Performance tuning
  (setq tramp-verbose 1
        tramp-chunksize 65536
        tramp-connection-timeout 10
        tramp-copy-size-limit nil)

  ;; Disable VC for remote files
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  ;; Disable heavy modes for remote files
  (add-hook 'find-file-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (setq-local vc-handled-backends nil)
                (setq-local create-lockfiles nil)
                (when (bound-and-true-p diff-hl-mode) (diff-hl-mode -1))))))

;; ============================================================================
;; 7. GIT INTEGRATION (MAGIT, FORK-LIKE UX)
;; ============================================================================

(my/load-feature "git"
  (require 'transient)

  (use-package magit
    :commands (magit-status magit-status-setup-buffer magit-dispatch)
    :init
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
          magit-bury-buffer-function #'magit-restore-window-configuration))

  (use-package magit-gitflow
    :after magit
    :hook (magit-mode . turn-on-magit-gitflow))

  (use-package pinentry
    :init (setq epg-pinentry-mode 'loopback)
    :config (pinentry-start))

  (use-package blamer
    :defer t
    :custom (blamer-idle-time 0.3) (blamer-min-offset 70)
    :config (global-blamer-mode 1))

  (use-package magit-delta
    :after magit
    :hook (magit-mode . magit-delta-mode))

  (use-package diff-hl
    :init
    (add-hook 'prog-mode-hook #'diff-hl-mode)
    (add-hook 'text-mode-hook #'diff-hl-mode)
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
    :config
    (with-eval-after-load 'magit
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

  ;; Fork-like helpers
  (defun fork-git-open-status ()
    "Open Magit status for current project."
    (interactive)
    (let ((root (or (ignore-errors (magit-toplevel))
                    (when-let* ((proj (project-current))) (project-root proj))
                    default-directory)))
      (magit-status-setup-buffer root)))

  (defun fork-git-blame-toggle ()
    "Toggle Magit blame."
    (interactive)
    (require 'magit)
    (if (bound-and-true-p magit-blame-mode)
        (magit-blame-quit)
      (magit-blame-addition)))

  (defun fork-git-file-history ()
    "Show Git history for current file."
    (interactive)
    (require 'magit)
    (call-interactively 'magit-log-buffer-file))

  (defun fork-git-branch-graph ()
    "Show branch graph."
    (interactive)
    (require 'magit)
    (let ((magit-log-arguments '("--graph" "--decorate" "--date-order")))
      (magit-log-all '())))

  (defun fork-git-show-inline-commit ()
    "Show inline commit details."
    (interactive)
    (require 'blamer)
    (call-interactively 'blamer-show-commit-info))

  (defun fork-git-magit-delta-toggle ()
    "Toggle delta diffs."
    (interactive)
    (require 'magit-delta)
    (if (bound-and-true-p magit-delta-mode)
        (magit-delta-mode -1)
      (magit-delta-mode 1)))

  ;; Hunk navigation
  (defun my/goto-next-hunk ()
    "Go to next git diff hunk."
    (interactive)
    (if (fboundp 'diff-hl-next-hunk) (diff-hl-next-hunk)
      (message "diff-hl not available")))

  (defun my/goto-prev-hunk ()
    "Go to previous git diff hunk."
    (interactive)
    (if (fboundp 'diff-hl-previous-hunk) (diff-hl-previous-hunk)
      (message "diff-hl not available")))

  (defun my/show-hunk-diff ()
    "Show diff for current hunk."
    (interactive)
    (if (fboundp 'diff-hl-diff-goto-hunk) (diff-hl-diff-goto-hunk)
      (message "diff-hl not available")))

  ;; Repository dashboard
  (defvar fork-git-repo-dashboard-buffer-name "*Fork Git Repositories*")

  (defun fork-git-open-repo-dashboard ()
    "Open repository dashboard."
    (interactive)
    (unless magit-repository-directories
      (user-error "Set `magit-repository-directories' first"))
    (require 'magit-repos)
    (let ((buf (get-buffer-create fork-git-repo-dashboard-buffer-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Git Repositories\n\n")
          (dolist (repo (magit-list-repos))
            (insert (format "  %s\n" (abbreviate-file-name repo))))
          (goto-char (point-min)))
        (special-mode))
      (pop-to-buffer buf))))

;; ============================================================================
;; 8. LANGUAGES AND LSP
;; ============================================================================

(my/load-feature "lsp"
  (use-package lsp-mode
    :commands lsp-deferred
    :init
    (setq lsp-warn-no-matched-clients nil
          lsp-headerline-breadcrumb-enable t
          lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)))

  (use-package lsp-ui
    :after lsp-mode
    :init
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-position 'at-point
          lsp-ui-doc-show-with-cursor t
          lsp-ui-sideline-enable t
          lsp-ui-sideline-show-code-actions t
          lsp-ui-sideline-show-diagnostics t))

  ;; LSP preferences
  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :capf
          lsp-prefer-flymake t)))

(my/load-feature "treesit"
  (use-package treesit-auto
    :if (and (fboundp 'treesit-available-p) (treesit-available-p))
    :init
    (setq treesit-auto-install nil)  ; grammars pre-compiled by install-emacs.sh
    (setq treesit-extra-load-path
          (list (expand-file-name "tree-sitter" user-emacs-directory)))
    ;; Pin grammars to version 14
    (setq treesit-language-source-alist
          '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
            (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
            (json "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
            (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
            (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3")))
    ;; Mode remapping
    (dolist (pair '((python-mode . python-ts-mode)
                    (js-mode . js-ts-mode)
                    (typescript-mode . typescript-ts-mode)
                    (go-mode . go-ts-mode)
                    (rust-mode . rust-ts-mode)
                    (bash-mode . bash-ts-mode)))
      (add-to-list 'major-mode-remap-alist pair))))

  ;; LSP hooks for ts-modes
  (dolist (mode '(python-ts-mode js-ts-mode typescript-ts-mode tsx-ts-mode
                  go-ts-mode rust-ts-mode bash-ts-mode json-ts-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'lsp-deferred))

(my/load-feature "web-mode"
  (use-package web-mode
    :mode (("\\.html?\\'" . web-mode)
           ("\\.jsx\\'" . web-mode))
    :init (add-hook 'web-mode-hook #'lsp-deferred)))

(my/load-feature "yaml-mode"
  (use-package yaml-mode :mode ("\\.ya?ml\\'" . yaml-mode)))

;; ============================================================================
;; 9. PYTHON IDE
;; ============================================================================

(my/load-feature "python"
  (use-package lsp-pyright :after lsp-mode)
  (with-eval-after-load 'lsp-mode (require 'lsp-pyright))

  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'python-ts-mode-hook #'lsp-deferred)

  ;; Format on save
  (defun my/python-lsp-format+imports ()
    (when (or (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode))
      (lsp-format-buffer)
      (lsp-organize-imports)))
  (add-hook 'before-save-hook #'my/python-lsp-format+imports)

  ;; Debugging
  (use-package dap-mode
    :after lsp-mode
    :init
    (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip))
    :config
    (require 'dap-python)
    (setq dap-python-debugger 'debugpy)
    (dap-ui-mode 1)
    (dap-tooltip-mode 1))

  (use-package python-pytest
    :config (setq python-pytest-executable "uv run pytest")))

;; ============================================================================
;; 10. MARKDOWN
;; ============================================================================

(my/load-feature "markdown"
  (use-package markdown-mode
    :mode (("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode)
           ("README\\.md\\'" . gfm-mode))
    :init
    (setq markdown-command "pandoc"
          markdown-enable-math t
          markdown-fontify-code-blocks-natively t)))

;; ============================================================================
;; 11. LATEX WORKSPACE
;; ============================================================================

(my/load-feature "latex"
  (require 'doc-view)
  (setq doc-view-continuous t
        doc-view-resolution 180
        doc-view-use-scaling t)

  (use-package tex
    :ensure auctex
    :defer t
    :mode ("\\.tex\\'" . LaTeX-mode)
    :hook ((LaTeX-mode . visual-line-mode)
           (LaTeX-mode . flyspell-mode)
           (LaTeX-mode . LaTeX-math-mode)
           (LaTeX-mode . TeX-source-correlate-mode))
    :init
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-PDF-mode t
          TeX-source-correlate-start-server nil
          TeX-source-correlate-method 'synctex))

  (defun my/latex-workspace ()
    "Open LaTeX workspace layout."
    (interactive)
    (message "LaTeX workspace: use C-c l in .tex files")))

;; ============================================================================
;; 12. CODE NAVIGATION AND SYNTAX
;; ============================================================================

(my/load-feature "code-navigation"
  (which-function-mode 1)

  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  (use-package dumb-jump
    :init
    (setq dumb-jump-prefer-searcher 'rg)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

  (use-package multiple-cursors
    :bind (("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c C->" . mc/mark-all-like-this))
    :init (setq mc/always-run-for-all t))

  (use-package editorconfig :config (editorconfig-mode 1))

  (use-package flymake :ensure nil :init (add-hook 'prog-mode-hook #'flymake-mode))

  (use-package vundo
    :init (setq vundo-glyph-alist vundo-unicode-symbols)
    :config
    (define-key vundo-mode-map (kbd "h") #'vundo-backward)
    (define-key vundo-mode-map (kbd "l") #'vundo-forward)
    (define-key vundo-mode-map (kbd "j") #'vundo-next)
    (define-key vundo-mode-map (kbd "k") #'vundo-previous))

  (setq imenu-auto-rescan t imenu-use-popup-menu nil))

;; ============================================================================
;; 13. KEYBINDINGS
;; ============================================================================

(my/load-feature "keybindings"
  ;; macOS-style shortcuts (Super = Cmd)
  (defun my/project-search-dwim ()
    "Search in project using consult-ripgrep."
    (interactive)
    (let* ((proj (project-current))
           (dir (if proj (project-root proj) default-directory)))
      (consult-ripgrep dir)))

  (global-set-key (kbd "s-F") #'my/project-search-dwim)
  (global-set-key (kbd "s-P") #'execute-extended-command)
  (global-set-key (kbd "s-p") #'project-find-file)
  (global-set-key (kbd "s-b") #'my/toggle-explorer)
  (global-set-key (kbd "s-`") #'my/toggle-vterm)
  (global-set-key (kbd "s-E") #'my/focus-explorer)
  (global-set-key (kbd "s-G") #'magit-status)
  (global-set-key (kbd "s-d") #'mc/mark-next-like-this)

  ;; Git hunk navigation
  (global-set-key (kbd "C-c [h") #'my/goto-prev-hunk)
  (global-set-key (kbd "C-c ]h") #'my/goto-next-hunk)
  (global-set-key (kbd "C-c =") #'my/show-hunk-diff)

  ;; Smart navigation
  (defun my/goto-definition-smart ()
    "Go to definition via LSP or dumb-jump."
    (interactive)
    (cond
     ((and (bound-and-true-p lsp-mode) (fboundp 'lsp-feature?) (lsp-feature? :definition))
      (call-interactively 'xref-find-definitions))
     ((fboundp 'dumb-jump-go) (call-interactively 'dumb-jump-go))
     (t (call-interactively 'xref-find-definitions))))

  (defun my/find-references-smart ()
    "Find references via LSP or ripgrep."
    (interactive)
    (cond
     ((and (bound-and-true-p lsp-mode) (fboundp 'lsp-feature?) (lsp-feature? :references))
      (call-interactively 'xref-find-references))
     ((fboundp 'consult-ripgrep)
      (let ((sym (or (thing-at-point 'symbol t) "")))
        (consult-ripgrep nil sym)))
     (t (message "No references backend available"))))

  ;; C-c global shortcuts
  (global-set-key (kbd "C-c e") #'my/focus-explorer)
  (global-set-key (kbd "C-c v") #'my/toggle-vterm)
  (global-set-key (kbd "C-c g") #'fork-git-open-repo-dashboard)
  (global-set-key (kbd "C-c /") #'consult-ripgrep)
  (global-set-key (kbd "C-c b") #'consult-buffer)
  (global-set-key (kbd "C-c p") #'project-find-file)
  (global-set-key (kbd "C-c o") #'find-file)
  (global-set-key (kbd "C-c u") #'vundo)
  (global-set-key (kbd "C-c [") #'tab-previous)
  (global-set-key (kbd "C-c ]") #'tab-next)
  (global-set-key (kbd "C-c n") #'tab-new)
  (global-set-key (kbd "C-c x") #'tab-close)
  (global-set-key (kbd "C-c y") #'fork-git-show-inline-commit)
  (global-set-key (kbd "C-c d") #'my/goto-definition-smart)
  (global-set-key (kbd "C-c j") #'consult-imenu)

  ;; Diagnostics
  (defun my/open-diagnostics ()
    (interactive)
    (cond
     ((fboundp 'consult-flymake) (consult-flymake))
     ((fboundp 'flymake-show-buffer-diagnostics) (flymake-show-buffer-diagnostics))
     (t (user-error "No diagnostics UI available"))))
  (global-set-key (kbd "C-c !") #'my/open-diagnostics)

  ;; vterm keybindings
  (with-eval-after-load 'vterm
    (let ((map vterm-mode-map))
      (define-key map (kbd "C-c o") #'find-file)
      (define-key map (kbd "C-y") #'my/vterm-clipboard-yank)))

  ;; LSP keybindings
  (with-eval-after-load 'lsp-mode
    (define-key lsp-mode-map (kbd "C-c r") #'my/find-references-smart)
    (define-key lsp-mode-map (kbd "C-c .") #'lsp-execute-code-action)
    (define-key lsp-mode-map (kbd "C-c f") #'lsp-format-buffer)
    (define-key lsp-mode-map (kbd "C-c i") #'lsp-organize-imports))

  ;; Dired keybindings
  (with-eval-after-load 'dired
    (let ((map dired-mode-map))
      (define-key map (kbd "C-c e") #'my/focus-explorer)
      (define-key map (kbd "C-c o") #'dired-find-file)
      (define-key map (kbd "C-c c") #'dired-do-copy)
      (define-key map (kbd "C-c r") #'dired-do-rename)
      (define-key map (kbd "C-c +") #'dired-create-directory)
      (define-key map (kbd "C-c d") #'dired-do-delete)
      (define-key map (kbd "C-c p") #'tc/dired-toggle-preview)
      (define-key map (kbd "C-c 2") #'tc/dired-two-panes)))

  ;; Magit keybindings
  (with-eval-after-load 'magit
    (let ((map magit-mode-map))
      (define-key map (kbd "C-c g") #'fork-git-open-repo-dashboard)
      (define-key map (kbd "C-c b") #'fork-git-branch-graph)
      (define-key map (kbd "C-c h") #'fork-git-file-history)
      (define-key map (kbd "C-c B") #'fork-git-blame-toggle)
      (define-key map (kbd "C-c d") #'fork-git-magit-delta-toggle)))

  ;; Python keybindings
  (with-eval-after-load 'python
    (define-key python-mode-map (kbd "C-c t") #'python-pytest))

  ;; Markdown keybindings
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c l") #'markdown-live-preview-mode)))

;; ============================================================================
;; 14. UTILITY FUNCTIONS
;; ============================================================================

(my/load-feature "utilities"
  (defun my/project-compile ()
    "Run compile in project root with smart default."
    (interactive)
    (let* ((project (project-current t))
           (default-directory (project-root project))
           (cmd (cond
                 ((file-exists-p "Makefile") "make -k")
                 ((file-exists-p "package.json") "npm run build")
                 ((file-exists-p "Cargo.toml") "cargo build")
                 ((file-exists-p "go.mod") "go build ./...")
                 ((file-exists-p "pyproject.toml") "uv run python -m pytest")
                 (t compile-command))))
      (compile cmd)))
  (global-set-key (kbd "C-c B") #'my/project-compile)

  (defun my/toggle-soft-wrap-global ()
    "Toggle soft wrapping globally."
    (interactive)
    (if global-visual-line-mode
        (global-visual-line-mode -1)
      (global-visual-line-mode 1)))
  (global-set-key (kbd "C-c w") #'my/toggle-soft-wrap-global))

;;; post-init.el ends here
