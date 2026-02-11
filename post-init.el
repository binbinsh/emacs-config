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

;; Set SSH_AUTH_SOCK for 1Password immediately
(when (eq system-type 'darwin)
  (let ((sock (expand-file-name "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock")))
    (when (file-exists-p sock)
      (setenv "SSH_AUTH_SOCK" sock))))

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

;; hl-line 配置已移至 monokai-light-theme.el

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
    (setq vertico-cycle t
          vertico-preselect 'prompt))
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
  ;; Use nerd-icons for corfu completion icons
  (use-package nerd-icons-corfu
    :after corfu
    :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

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
    (hl-line-mode 1)
    ;; 增加行间距
    (setq-local line-spacing 0.5)
    ;; GUI 模式下使用 JetBrains Mono Light
    (when (display-graphic-p)
      (face-remap-add-relative 'default :family "JetBrains Mono" :weight 'light :height 1.3))))

  (use-package diredfl :hook (dired-mode . diredfl-mode))
  (use-package dired-git-info :defer t :commands dired-git-info-mode)
  (use-package peep-dired :defer t :commands peep-dired)

  ;; Quick preview toggle
  (defun tc/dired-toggle-preview ()
    "Toggle quick preview in Dirvish or Dired."
    (interactive)
    (cond
     ((and (fboundp 'dirvish-curr) (dirvish-curr) (fboundp 'dirvish-layout-toggle))
      (dirvish-layout-toggle))
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

(my/load-feature "treemacs"
  ;; Treemacs: modern IDE-style file explorer
  (use-package treemacs
    :defer t
    :init
    (setq treemacs-persist-file (expand-file-name "treemacs-persist" my-emacs-cache-directory)
          treemacs-last-error-persist-file (expand-file-name "treemacs-last-error-persist" my-emacs-cache-directory))
    :config
    ;; Visual settings
    (setq treemacs-width 35
          treemacs-width-is-initially-locked nil
          treemacs-indentation 2
          treemacs-indentation-string " "
          treemacs-show-hidden-files t
          treemacs-sorting 'alphabetic-asc
          treemacs-follow-after-init t
          treemacs-expand-after-init t
          treemacs-is-never-other-window nil  ;; Allow switching to treemacs with C-x o
          treemacs-silent-refresh t
          treemacs-silent-filewatch t
          treemacs-no-png-images nil
          treemacs-collapse-dirs (if treemacs-python-executable 3 0)
          treemacs-file-event-delay 1000
          treemacs-file-follow-delay 0.1
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow nil
          treemacs-goto-tag-strategy 'refetch-index
          treemacs-show-cursor nil
          treemacs-user-mode-line-format nil
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes t
          treemacs-directory-name-transformer #'identity
          treemacs-file-name-transformer #'identity)

    ;; Enable modes
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-hide-gitignored-files-mode nil)

    ;; Git integration (deferred for performance)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t) (treemacs-git-mode 'deferred))
      (`(t . _) (treemacs-git-mode 'simple)))

    ;; VSCode-style keybindings in treemacs buffer
    (define-key treemacs-mode-map (kbd "a") #'treemacs-create-file)
    (define-key treemacs-mode-map (kbd "A") #'treemacs-create-dir)
    (define-key treemacs-mode-map (kbd "r") #'treemacs-rename-file)
    (define-key treemacs-mode-map (kbd "d") #'treemacs-delete-file)
    (define-key treemacs-mode-map (kbd "m") #'treemacs-move-file)
    (define-key treemacs-mode-map (kbd "c") #'treemacs-copy-file)
    (define-key treemacs-mode-map (kbd "y") #'treemacs-copy-path-at-point)
    (define-key treemacs-mode-map (kbd "Y") #'treemacs-copy-project-root)
    (define-key treemacs-mode-map (kbd "o") #'treemacs-visit-node-ace)
    (define-key treemacs-mode-map (kbd "O") #'treemacs-visit-node-ace-horizontal-split)
    (define-key treemacs-mode-map (kbd "v") #'treemacs-visit-node-ace-vertical-split)
    (define-key treemacs-mode-map (kbd "x") #'treemacs-collapse-parent-node)
    (define-key treemacs-mode-map (kbd "u") #'treemacs-goto-parent-node)
    (define-key treemacs-mode-map (kbd "R") #'treemacs-refresh)
    (define-key treemacs-mode-map (kbd "H") #'treemacs-toggle-show-dotfiles)
    (define-key treemacs-mode-map (kbd "w") #'treemacs-set-width)
    (define-key treemacs-mode-map (kbd "P") #'treemacs-peek-mode))

  ;; Nerd icons theme (beautiful icons)
  (use-package treemacs-nerd-icons
    :after treemacs
    :config
    (treemacs-load-theme "nerd-icons"))

  ;; Magit integration
  (use-package treemacs-magit
    :after (treemacs magit))

  ;; Project.el integration
  (use-package treemacs-project-follow-mode
    :ensure nil
    :after treemacs
    :config
    (treemacs-project-follow-mode t))

  ;; Toggle/focus functions
  (defun my/toggle-explorer ()
    "Toggle Treemacs sidebar."
    (interactive)
    (if (and (fboundp 'treemacs-current-visibility)
             (eq (treemacs-current-visibility) 'visible))
        (treemacs)
      (let* ((project (ignore-errors (project-current)))
             (root (if project (project-root project) default-directory)))
        (treemacs-add-and-display-current-project-exclusively))))

  (defun my/focus-explorer ()
    "Focus the Treemacs sidebar, opening it if necessary."
    (interactive)
    (pcase (treemacs-current-visibility)
      ('visible (treemacs-select-window))
      (_ (my/toggle-explorer))))

  ;; Auto-start treemacs on startup (after frame is ready)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (display-graphic-p)
                (run-with-idle-timer
                 0.5 nil
                 (lambda ()
                   (unless (eq (treemacs-current-visibility) 'visible)
                     (save-selected-window
                       (treemacs-add-and-display-current-project-exclusively)))))))))

;; ============================================================================
;; 6. TERMINAL AND TRAMP
;; ============================================================================

(my/load-feature "terminal"
  ;; vterm terminal
  (use-package vterm
    :commands (vterm vterm-mode)
    :config
    (setq vterm-kill-buffer-on-exit t)
    (setq vterm-max-scrollback 10000)
    (setq vterm-shell my/terminal-shell))

  ;; Disable visual clutter in terminal
  (add-hook 'vterm-mode-hook
            (lambda ()
              (display-fill-column-indicator-mode -1)
              (display-line-numbers-mode -1)
              (setq-local global-hl-line-mode nil)))

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

  ;; Disable ControlMaster - let SSH handle it via config if needed
  (setq tramp-use-ssh-controlmaster-options nil)

  ;; Cache settings - aggressive caching for speed
  (setq remote-file-name-inhibit-cache nil
        tramp-completion-reread-directory-timeout nil
        tramp-cache-read-persistent-data t)

  ;; Performance tuning
  (setq tramp-verbose 1
        tramp-chunksize 65536
        tramp-connection-timeout 30
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

;; Quick remote dired (outside of feature block for global availability)
(defun my/ssh-hosts ()
  "Get SSH hosts from ~/.ssh/config using TRAMP's parser."
  (require 'tramp)
  (let ((config (expand-file-name "~/.ssh/config")))
    (when (file-exists-p config)
      (delq nil
            (mapcar (lambda (entry)
                      (let ((host (cadr entry)))
                        (when (and host (not (string-match-p "[*?]" host)))
                          host)))
                    (tramp-parse-sconfig config))))))

(defun my/remote-dired (host &optional path)
  "Open dired on remote HOST at PATH (defaults to home)."
  (interactive
   (list (completing-read "SSH Host: " (my/ssh-hosts) nil nil)))
  ;; Use sshx method - doesn't run login shell, avoids prompt matching issues
  (let ((dir (format "/sshx:%s:%s" host (or path "~/"))))
    (if (fboundp 'dirvish)
        (dirvish dir)
      (dired dir))))

(global-set-key (kbd "C-c h") #'my/remote-dired)

;; ============================================================================
;; 7. GIT INTEGRATION (MAGIT)
;; ============================================================================

(my/load-feature "git"
  (require 'transient)

  (use-package magit
    :commands (magit-status magit-status-setup-buffer magit-dispatch)
    :init
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
          magit-bury-buffer-function #'magit-restore-window-configuration)
    :config
    ;; Performance optimizations
    (setq magit-diff-refine-hunk nil              ; 不显示单词级别的精细 diff
          magit-diff-highlight-hunk-body nil      ; 不高亮 hunk 内容
          magit-section-highlight-hook nil        ; 禁用 section 高亮 hook
          magit-section-unhighlight-hook nil      ; 禁用 section 取消高亮 hook
          magit-refresh-status-buffer nil         ; 切换 buffer 时不自动刷新
          magit-revision-insert-related-refs nil  ; 不显示相关 refs
          magit-save-repository-buffers 'dontask  ; 自动保存不询问
          magit-diff-paint-whitespace nil         ; 不高亮空白字符
          auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p))

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

  ;; magit-delta 会导致严重性能问题 - 每次光标移动都触发 delta 渲染
  ;; 如需使用，可以手动 M-x magit-delta-mode 开启
  (use-package magit-delta
    :after magit
    :commands magit-delta-mode)

  (use-package diff-hl
    :init
    (add-hook 'prog-mode-hook #'diff-hl-mode)
    (add-hook 'text-mode-hook #'diff-hl-mode)
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
    :config
    (with-eval-after-load 'magit
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

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
      (message "diff-hl not available"))))

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

  ;; Open binary files with system default apps (handles TRAMP remote files)
  (defvar my/external-file-extensions
    '("pdf" "wav" "mp3" "flac" "ogg" "aac" "m4a" "wma"
      "mp4" "mkv" "avi" "mov" "wmv" "webm" "flv"
      "docx" "doc" "xlsx" "xls" "pptx" "ppt")
    "File extensions to open with system default application.")

  (defun my/open-externally (file)
    "Open FILE with system default app. For remote files, copy to local temp first."
    (interactive (list (or (dired-get-filename nil t) (buffer-file-name) (read-file-name "File: "))))
    (let* ((local-file
            (if (file-remote-p file)
                (let* ((ext (file-name-extension file))
                       (tmp (make-temp-file "emacs-remote-" nil (when ext (concat "." ext)))))
                  (copy-file file tmp t)
                  tmp)
              (expand-file-name file))))
      (start-process "open-external" nil "open" local-file)))

  (defun my/external-file-p (filename)
    "Return non-nil if FILENAME should be opened externally."
    (member (downcase (or (file-name-extension filename) ""))
            my/external-file-extensions))

  ;; Advise find-file to open matching files externally
  (define-advice find-file (:around (orig-fn filename &rest args) external-open)
    (if (my/external-file-p filename)
        (my/open-externally filename)
      (apply orig-fn filename args)))

  ;; Make dired `!' smart: remote files auto-copy to local and open
  (defun my/dired-do-open (arg)
    "Open marked (or current) files with system app. Handles remote files."
    (interactive "P")
    (let ((files (dired-get-marked-files t arg)))
      (if (file-remote-p default-directory)
          (dolist (f files) (my/open-externally f))
        (dolist (f files)
          (start-process "open-external" nil "open" (expand-file-name f))))))

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
;; 12. JSONL PREVIEW (auto pretty-print current line)
;; ============================================================================

(my/load-feature "jsonl-preview"
  (defvar jsonl-preview--buffer-name "*JSONL Preview*")

  (defun jsonl-preview--ensure-window ()
    "Ensure preview window is visible, side based on frame size."
    (unless (get-buffer-window jsonl-preview--buffer-name)
      (let* ((width (frame-width))
             (height (frame-height))
             ;; Use right side if wide enough (>120 cols), otherwise bottom
             (side (if (> width 120) 'right 'bottom))
             (size (if (eq side 'right) 0.4 0.4)))
        (display-buffer (get-buffer-create jsonl-preview--buffer-name)
                        `(display-buffer-in-side-window
                          (side . ,side)
                          ,(if (eq side 'right)
                               `(window-width . ,size)
                             `(window-height . ,size)))))))

  (defun jsonl-preview--parse-json (str)
    "Parse JSON string STR, trying multiple methods."
    (condition-case nil
        ;; Try native json-parse-string first
        (json-parse-string str :object-type 'alist)
      (error
       ;; Fallback to json-read-from-string
       (condition-case nil
           (let ((json-object-type 'alist)
                 (json-array-type 'vector)
                 (json-key-type 'string))
             (json-read-from-string str))
         (error nil)))))

  (defun jsonl-preview--format-json (obj)
    "Format parsed JSON OBJ as pretty string."
    (with-temp-buffer
      (let ((json-encoding-pretty-print t)
            (json-encoding-default-indentation "  "))
        (insert (json-encode obj)))
      (buffer-string)))

  (defun jsonl-preview--update ()
    "Update the preview buffer with pretty JSON of current line."
    (when (and (bound-and-true-p jsonl-preview-mode)
               (string-suffix-p ".jsonl" (or buffer-file-name "") t))
      (let* ((line (string-trim (or (thing-at-point 'line t) ""))))
        (jsonl-preview--ensure-window)
        (with-current-buffer (get-buffer-create jsonl-preview--buffer-name)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (if (or (string-empty-p line) (< (length line) 2))
                (insert "(empty line)")
              (if-let ((parsed (jsonl-preview--parse-json line)))
                  (insert (jsonl-preview--format-json parsed))
                (insert "(invalid JSON)\n\nFirst 200 chars:\n"
                        (substring line 0 (min 200 (length line))))))
            (goto-char (point-min))
            ;; Enable syntax highlighting
            (unless (derived-mode-p 'json-ts-mode 'json-mode)
              (if (fboundp 'json-ts-mode)
                  (json-ts-mode)
                (when (fboundp 'json-mode)
                  (json-mode))))
            (setq buffer-read-only t))))))

  (defun jsonl-preview--hide ()
    "Hide the preview window."
    (when-let ((win (get-buffer-window jsonl-preview--buffer-name)))
      (delete-window win)))

  (define-minor-mode jsonl-preview-mode
    "Auto preview JSONL lines as pretty JSON in side window."
    :lighter " JP"
    (if jsonl-preview-mode
        (progn
          (jsonl-preview--ensure-window)
          (add-hook 'post-command-hook #'jsonl-preview--update nil t)
          (jsonl-preview--update))
      (remove-hook 'post-command-hook #'jsonl-preview--update t)
      (jsonl-preview--hide)))

  ;; Auto-enable for .jsonl files
  (add-hook 'json-ts-mode-hook
            (lambda ()
              (when (string-suffix-p ".jsonl" (or buffer-file-name "") t)
                (jsonl-preview-mode 1))))
  (add-hook 'find-file-hook
            (lambda ()
              (when (string-suffix-p ".jsonl" (or buffer-file-name "") t)
                (jsonl-preview-mode 1)))))

;; ============================================================================
;; 13. CODE NAVIGATION AND SYNTAX
;; ============================================================================

(my/load-feature "code-navigation"
  (which-function-mode 1)

  ;; 1. 全局禁用 tab，使用空格
  (setq-default indent-tabs-mode nil)

  ;; 2. 自动检测已有文件的缩进风格 (dtrt-indent)
  (use-package dtrt-indent
    :diminish
    :config
    (dtrt-indent-global-mode 1))

  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  (use-package dumb-jump
    :init
    (setq dumb-jump-prefer-searcher 'rg)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

  (use-package ast-grep
    :vc (:url "https://github.com/SunskyXH/ast-grep.el")
    :commands (ast-grep-search ast-grep-project ast-grep-directory))

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

  (defun my/ast-grep-search ()
    "Run ast-grep search in project or current directory."
    (interactive)
    (let* ((proj (project-current))
           (dir (if proj (project-root proj) default-directory))
           (pattern (read-string "ast-grep pattern: " (thing-at-point 'symbol t)
                                 'ast-grep-history)))
      (ast-grep-search pattern dir)))

  (global-set-key (kbd "s-F") #'my/project-search-dwim)
  (global-set-key (kbd "s-P") #'execute-extended-command)
  (global-set-key (kbd "s-p") #'project-find-file)
  (global-set-key (kbd "s-b") #'my/toggle-explorer)
  (global-set-key (kbd "s-E") #'my/focus-explorer)
  (global-set-key (kbd "s-G") #'magit-status)
  (global-set-key (kbd "C-c g") #'magit-status)
  (global-set-key (kbd "s-d") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-x d") #'dirvish-dwim)

  ;; Terminal super key bindings via CSI u sequences (for Ghostty SSH)
  (unless (display-graphic-p)
    (define-key input-decode-map "\e[70;5u" (kbd "s-F"))
    (define-key input-decode-map "\e[80;5u" (kbd "s-P"))
    (define-key input-decode-map "\e[112;3u" (kbd "s-p"))
    (define-key input-decode-map "\e[98;3u" (kbd "s-b"))
    (define-key input-decode-map "\e[96;3u" (kbd "s-`"))
    (define-key input-decode-map "\e[69;5u" (kbd "s-E"))
    (define-key input-decode-map "\e[71;5u" (kbd "s-G"))
    (define-key input-decode-map "\e[100;3u" (kbd "s-d")))

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
  (global-set-key (kbd "C-c G") #'magit-list-repositories)
  (global-set-key (kbd "C-c /") #'consult-ripgrep)
  (global-set-key (kbd "C-c ?") #'my/ast-grep-search)
  (global-set-key (kbd "C-c b") #'consult-buffer)
  (global-set-key (kbd "C-c p") #'project-find-file)
  (global-set-key (kbd "C-c o") #'find-file)
  (global-set-key (kbd "C-c u") #'vundo)
  (global-set-key (kbd "C-c [") #'tab-previous)
  (global-set-key (kbd "C-c ]") #'tab-next)
  (global-set-key (kbd "C-c n") #'tab-new)
  (global-set-key (kbd "C-c x") #'tab-close)
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
    ;; Disable default C-x g, use C-c g instead
    (global-unset-key (kbd "C-x g"))
    (let ((map magit-mode-map))
      (define-key map (kbd "C-c G") #'magit-list-repositories)
      (define-key map (kbd "C-c b") #'magit-log-all)
      (define-key map (kbd "C-c B") #'magit-blame)))

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
