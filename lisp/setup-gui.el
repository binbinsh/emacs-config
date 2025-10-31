;; Core GUI baseline (Cursor-like)
(setq inhibit-startup-screen t)

;; Theme (light)
(use-package modus-themes
  :init (load-theme 'modus-operandi t))

;; Frame behavior and size
(setq frame-resize-pixelwise t)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Ensure new GUI frames (e.g., emacsclient) start maximized
(add-hook 'after-make-frame-functions
  (lambda (frame)
    (when (display-graphic-p frame)
      (set-frame-parameter frame 'fullscreen 'maximized))))

;; Clean chrome
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Fonts (optimize English + Chinese)
(require 'cl-lib)
(defun my/find-first-font (fonts)
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(defun my/apply-fonts (&optional frame)
  "Apply preferred English/CJK/Emoji fonts to FRAME (or current)."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (let* ((default-size 15)
             (english (or (my/find-first-font
                           '("JetBrainsMono Nerd Font Mono" "JetBrainsMono Nerd Font" "JetBrainsMono NF"
                             "JetBrains Mono" "Fira Code" "Hack" "DejaVu Sans Mono"
                             "SF Mono" "Menlo" "Monaco"))
                          "Monaco"))
             (cjk (or (my/find-first-font
                       '("Noto Sans CJK SC" "Noto Sans CJK" "PingFang SC" "Hiragino Sans GB"))
                      "Noto Sans CJK SC"))
             (emoji (or (my/find-first-font '("Apple Color Emoji" "Noto Color Emoji"))
                        nil)))
        (set-face-attribute 'default nil :font (format "%s-%d" english default-size))
        (dolist (charset '(han kana cjk-misc bopomofo))
          (set-fontset-font t charset (font-spec :name cjk)))
        (when emoji
          (set-fontset-font t 'symbol (font-spec :name emoji) nil 'prepend))
        ;; Force nerd-icons to use the same mono font
        (setq nerd-icons-font-family english)))))

(add-hook 'emacs-startup-hook #'my/apply-fonts)
(add-hook 'after-make-frame-functions #'my/apply-fonts)

;; Modeline (status bar)
(use-package doom-modeline
  :init
  (setq doom-modeline-icon t)
  (setq doom-modeline-height 26)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  :config
  (doom-modeline-mode 1))

;; Keycast: show pressed keys and commands in the modeline (for demo/memory)
(use-package keycast
  :init
  ;; Expose keycast's formatted text via global-mode-string for modeline consumption
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  :config
  ;; Provide a simple global switch that updates the keycast mode-line each command.
  ;; This keeps it compatible with doom-modeline while remaining lightweight.
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (doom-modeline-friendly)."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook #'keycast--update t)
      (remove-hook 'pre-command-hook #'keycast--update))))

;; Integrate keycast into doom-modeline on the right end (after checker)
(with-eval-after-load 'doom-modeline
  (doom-modeline-def-modeline 'my-doom-modeline
    '(bar window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs check))
  (add-hook 'doom-modeline-mode-hook (lambda () (doom-modeline-set-modeline 'my-doom-modeline t))))

;; Enable at startup
(keycast-mode 1)

;; Tabs
(setq tab-bar-show 0)
(tab-bar-mode 1)

;; Larger tabs (increase size without changing spacing between tabs)
(with-eval-after-load 'tab-bar
  (set-face-attribute 'tab-bar nil :height 180)
  (set-face-attribute 'tab-bar-tab nil :height 180 :weight 'semibold)
  (set-face-attribute 'tab-bar-tab-inactive nil :height 180 :weight 'normal))

;; Smooth scrolling
(pixel-scroll-precision-mode 1)
(setq scroll-margin 8)
(setq scroll-conservatively 101)
(setq auto-window-vscroll nil)

;; Line numbers (avoid terminals)
(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))
(add-hook 'text-mode-hook (lambda () (display-line-numbers-mode 1)))
(add-hook 'term-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Column number
(setq column-number-mode t)

;; Fill column indicator (built-in)
(setq-default display-fill-column-indicator-column 120)
(global-display-fill-column-indicator-mode 1)

;; Disable indicator in vterm (terminal config also ensures this)
(add-hook 'vterm-mode-hook (lambda () (display-fill-column-indicator-mode -1)))

;; Current line and parens
(global-hl-line-mode 1)
(add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
(show-paren-mode 1)

;; Command hints
(use-package which-key
  :config (which-key-mode 1))

;; which-key as a posframe in GUI
(use-package which-key-posframe
  :if (display-graphic-p)
  :after which-key
  :config
  (which-key-posframe-mode 1))

;; Completion: Vertico + Orderless + Marginalia + Consult + Embark
(use-package vertico
  :init
  (vertico-mode 1)
  (savehist-mode 1)
  (setq vertico-cycle t))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :init
  (setq register-preview-delay 0.2
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window))

;; Consult + LSP integration: enhanced symbol/diagnostic search
(use-package consult-lsp
  :after (consult lsp-mode)
  :config
  ;; Use consult-lsp for workspace symbol search when invoking xref-find-apropos
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Corfu + Cape: in-buffer completion consistent with Vertico style
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
    (setq corfu-popupinfo-delay 0.15
          corfu-popupinfo-hide nil)))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword))

;; Floating minibuffer in GUI
(use-package mini-frame
  :if (display-graphic-p)
  :init
  (setq mini-frame-resize t
        mini-frame-create-lazy t
        mini-frame-show-parameters '((top . 0.18)
                                     (left . 0.5)
                                     (width . 0.6)
                                     (height . 15)
                                     (internal-border-width . 12)
                                     (undecorated . t)
                                     (child-frame-border-width . 1)))
  :config
  (mini-frame-mode 1))

;; Helpful: better help buffers
(use-package helpful
  :init
  (define-key help-map (kbd "f") #'helpful-callable)
  (define-key help-map (kbd "v") #'helpful-variable)
  (define-key help-map (kbd "k") #'helpful-key)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command))

;; Eldoc-Box: hover documentation in GUI childframe
(use-package eldoc-box
  :if (display-graphic-p)
  :hook ((prog-mode . eldoc-box-hover-mode))
  :init
  (setq eldoc-box-max-pixel-width 600
        eldoc-box-max-pixel-height 400))

;; Quality-of-life
(defalias 'yes-or-no-p 'y-or-n-p)
(recentf-mode 1)
(save-place-mode 1)
(window-divider-mode 1)
(setq window-divider-default-places 'right-only)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-right-width 1)

;; Diff highlights in fringe
(use-package diff-hl
  :init
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  (add-hook 'text-mode-hook #'diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; Vundo: tree-based undo visualizer
(use-package vundo
  :init
  (setq vundo-glyph-alist vundo-unicode-symbols)
  :config
  (define-key vundo-mode-map (kbd "h") #'vundo-backward)
  (define-key vundo-mode-map (kbd "l") #'vundo-forward)
  (define-key vundo-mode-map (kbd "j") #'vundo-next)
  (define-key vundo-mode-map (kbd "k") #'vundo-previous))

;; Kind icons in Corfu margin
(use-package kind-icon
  :after corfu
  :init
  (setq kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Explorer sidebar (Dirvish side) with icons
(use-package vscode-icon
  :if (display-graphic-p))

(use-package dirvish
  :after project
  :config
  (dirvish-override-dired-mode)
  ;; Only force vscode-icon in GUI; in TTY we fallback in setup-dired.el
  (when (display-graphic-p)
    (setq dirvish-attributes '(vscode-icon file-size file-time git-msg)
          dirvish-side-attributes '(vscode-icon file-size file-time git-msg))) )

(use-package dirvish-side
  :ensure nil
  :after dirvish
  :init
  (setq dirvish-side-display-alist '((side . left) (slot . -1))
        dirvish-side-width 48
        dirvish-side-window-parameters '((no-delete-other-windows . t)
                                         (no-other-window . t)))
  (defun my/dirvish-project-root ()
    "Open dirvish at project root or current directory."
    (let* ((project (ignore-errors (project-current)))
           (root (if project (project-root project) default-directory)))
      ;; If current buffer is a fullframe Dirvish session, create side from a
      ;; temporary non-Dirvish buffer to avoid the "Can not create side session here" guard.
      (let ((blocked (and (fboundp 'dirvish-curr)
                          (when-let ((dv (dirvish-curr))) (dv-curr-layout dv)))))
        (if blocked
            (with-temp-buffer (dirvish-side root))
          (dirvish-side root)))))
  (defun my/toggle-explorer ()
    "Toggle explorer like Cursor's sidebar (Dirvish side)."
    (interactive)
    (let ((w (and (fboundp 'dirvish-side--session-visible-p)
                  (dirvish-side--session-visible-p))))
      (if (and (windowp w) (window-live-p w))
          (with-selected-window w (dirvish-quit))
        (my/dirvish-project-root)))))


  (defun my/dirvish-side-window ()
    "Return the Dirvish side window if visible, else nil."
    (catch 'win
      (dolist (w (window-list))
        (with-current-buffer (window-buffer w)
          (when (and (derived-mode-p 'dirvish-directory-view-mode)
                     (eq (window-parameter w 'no-other-window) t))
            (throw 'win w))))
      nil))

  (defun my/focus-explorer ()
    (interactive)
    (let* ((side (my/dirvish-side-window))
           (parent-left nil)
           (parent-left-x nil)
           (dired-left nil)
           (dired-left-x nil))
      (dolist (w (window-list))
        (with-current-buffer (window-buffer w)
          (let ((x (car (window-edges w))))
            (cond
              ((derived-mode-p 'dirvish-directory-view-mode)
               (when (or (null parent-left-x) (< x parent-left-x))
                 (setq parent-left-x x parent-left w)))
              ((derived-mode-p 'dired-mode)
               (when (or (null dired-left-x) (< x dired-left-x))
                 (setq dired-left-x x dired-left w)))))))
      (cond
        ((and (windowp side) (window-live-p side))
         (select-window side))
        (t
         (ignore-errors (my/dirvish-project-root))
         (let ((w (or (my/dirvish-side-window) parent-left dired-left)))
           (when (and (windowp w) (window-live-p w))
             (select-window w)))))))


  ;; Keep explorer visible when creating/duplicating new tabs
  (with-eval-after-load 'tab-bar
    (defun my/ensure-explorer-on-new-tab (&rest _)
      (when (display-graphic-p)
        (let ((main (selected-window)))
          (run-at-time 0.01 nil
                       (lambda ()
                         (when (frame-live-p (selected-frame))
                           (my/dirvish-project-root)
                           (when (window-live-p main)
                             (select-window main)))))))
    (advice-add 'tab-bar-new-tab :after #'my/ensure-explorer-on-new-tab)
    (advice-add 'tab-new :after #'my/ensure-explorer-on-new-tab)
    (advice-add 'tab-duplicate :after #'my/ensure-explorer-on-new-tab))
  :config
  (require 'dirvish-side))

;; Integrated terminal toggle (bottom panel)
(defun my/toggle-vterm ()
  "Toggle a bottom vterm panel (approx. 30% height)."
  (interactive)
  (let* ((buf (get-buffer "*vterm*"))
         (win (and buf (get-buffer-window buf t))))
    (if (and buf win)
      (delete-window win)
      (let* ((target-lines (floor (* 0.3 (frame-height))))
             (new-win (split-window (selected-window) (- target-lines) 'below)))
        (select-window new-win)
        (vterm)))))

;; Keybindings moved to the unified Command leader (see setup-keys.el)

(provide 'setup-gui)
