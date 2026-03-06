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
;; 7.  Git Integration (Magit workflow)
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

;; Declared here so lexical-binding files can dynamically bind this variable
;; before `eat' is loaded.
(defvar eat-buffer-name nil)

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
     (let ((module-delay my/post-init-delay)
           (module-name ,name))
       (when (fboundp 'my/startup-profile-register-module)
         (my/startup-profile-register-module module-name module-delay))
       (run-with-idle-timer module-delay nil
                            (lambda ()
                              (let ((module-begin (current-time)))
                                (condition-case err
                                    (progn
                                      ,@body
                                      (when (fboundp 'my/startup-profile-record-module)
                                        (my/startup-profile-record-module
                                         module-name module-begin (current-time) 'ok)))
                                  (error
                                   (when (fboundp 'my/startup-profile-record-module)
                                     (my/startup-profile-record-module
                                      module-name module-begin (current-time) 'error))
                                   (message "Error loading %s: %s" module-name err)))))))))

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
  (defun my/dired--listing-switches (sort)
    "Return portable Dired switches for SORT.
Local directory-first ordering comes from `ls-lisp-dirs-first'."
    (pcase sort
      ('name "-la")
      ('mtime "-lat")
      ('size "-laS")
      (_ (error "Unsupported Dired sort state: %S" sort))))
  (defconst my/dired-listing-switches (my/dired--listing-switches 'name)
    "Portable Dired listing switches for local buffers.")
  ;; Dired usability
  (setq dired-dwim-target t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-listing-switches my/dired-listing-switches
        dired-use-ls-dired nil
        global-auto-revert-non-file-buffers t)
  ;; `dired-omit-files' is defined in dired-x, so set it only after load.
  (with-eval-after-load 'dired-x
    (setq dired-omit-files (concat dired-omit-files "\\|^\\.[^.].*")))
  (defconst my/dired-remote-listing-switches my/dired-listing-switches
    "Portable Dired listing switches for TRAMP buffers.")
  (defun my/dired-setup-remote-listing-compat ()
    "Use portable `ls' options on remote hosts."
    (when (file-remote-p default-directory)
      (setq-local dired-use-ls-dired nil)
      (setq-local dired-listing-switches my/dired-remote-listing-switches)))
  (add-hook 'dired-mode-hook #'auto-revert-mode)
  (add-hook 'dired-mode-hook #'my/dired-setup-remote-listing-compat)

  ;; Visuals
  (add-hook 'dired-mode-hook (lambda ()
    (when (fboundp 'dired-hide-details-mode) (dired-hide-details-mode 1))
    (when (require 'dired-x nil t) (dired-omit-mode 1))
    (hl-line-mode 1)
    ;; 增加行间距，但保持高亮行文字在垂直方向居中。
    ;; 使用 line-height 属性在上/下各加一半间距（line-spacing 只会加在下方）。
    (setq-local line-spacing nil)
    (when (display-graphic-p)
      (let ((inhibit-read-only t))
        (add-text-properties (point-min) (point-max)
                             '(line-height (1.3 1.6)))))
    ;; 保持全局默认字体，避免在 dired 里额外放大
    (when (display-graphic-p)
      (face-remap-add-relative 'default :height 1.0))))

  (defun my/dired-apply-line-height ()
    "Apply balanced line height for Dired listings."
    (when (display-graphic-p)
      (let ((inhibit-read-only t))
        (add-text-properties (point-min) (point-max)
                             '(line-height (1.3 1.6))))))
  (add-hook 'dired-after-readin-hook #'my/dired-apply-line-height)

  (require 'transient)

  (use-package diredfl :hook (dired-mode . diredfl-mode))
  (use-package dired-git-info :defer t :commands dired-git-info-mode)
  (use-package peep-dired :defer t :commands peep-dired)
  (use-package dired-ranger
    :after dired
    :commands (dired-ranger-copy dired-ranger-move dired-ranger-paste))

  (defcustom my/dired-remote-preview-max-bytes (* 128 1024 1024)
    "Max size of remote file copied locally for preview metadata/thumbnail."
    :type 'integer
    :group 'dired)

  (defcustom my/dired-session-auto-restore t
    "Whether to auto-restore last Dired/Dirvish session on startup."
    :type 'boolean
    :group 'dired)

  (defcustom my/dired-session-max-count 20
    "Max count of directories persisted in Dired/Dirvish session."
    :type 'integer
    :group 'dired)

  (defcustom my/dired-live-preview-idle-delay 0.12
    "Idle delay seconds before live preview refresh."
    :type 'number
    :group 'dired)

  (defcustom my/dired-preview-cache-max-entries 400
    "Maximum preview cache entries kept on disk."
    :type 'integer
    :group 'dired)

  (defcustom my/dired-preview-cache-ttl-seconds (* 14 24 3600)
    "TTL in seconds for preview cache files."
    :type 'integer
    :group 'dired)

  (defcustom my/dired-operation-queue-refresh-interval 0.6
    "Realtime refresh interval for operation queue buffer."
    :type 'number
    :group 'dired)

  (defcustom my/dired-follow-cursor t
    "Whether Dired should recenter to keep point visible while moving."
    :type 'boolean
    :group 'dired)

  (defvar my/dired-thumbnail-buffer-name "*Dired Thumbnail*")
  (defvar my/dired-metadata-buffer-name "*Dired Metadata*")
  (defvar my/dired-operation-queue-buffer-name "*Dired Operations*")
  (defvar my/dired-preview-cache-directory
    (expand-file-name "dirvish/preview-cache" my-emacs-cache-directory))
  (defvar my/dired-preview-hash-index-file
    (expand-file-name "dirvish/preview-hash-index.el" my-emacs-cache-directory))
  (defvar my/dired-preview-hash-index (make-hash-table :test 'equal))
  (defvar my/dired-session-file
    (expand-file-name "dirvish/session.el" my-emacs-cache-directory))
  (defvar my/dired-tags-file
    (expand-file-name "dirvish/tags.el" my-emacs-cache-directory))
  (defvar my/dired-session-dirs nil)
  (defvar my/dired-tags-table (make-hash-table :test 'equal))
  (defvar-local my/dired--live-preview-last-file nil)
  (defvar-local my/dired--live-preview-timer nil)
  (defvar-local my/dired--interaction-last-point nil)
  (defvar-local my/dired-operation-queue--timer nil)
  (with-eval-after-load 'dirvish-quick-access
    (setq dirvish-quick-access-entries
          `(("h" ,(expand-file-name "~/") "Home")
            ("d" ,(expand-file-name "~/Downloads/") "Downloads")
            ("c" ,(expand-file-name "~/.emacs.d/") "Emacs Config")
            ("p" ,(expand-file-name "~/Projects/") "Projects")
            ("t" ,temporary-file-directory "Temp"))))

  (defconst my/dired-video-extensions
    '("mp4" "mkv" "mov" "avi" "webm" "flv" "wmv" "m4v")
    "Known video extensions for thumbnail/metadata.")

  (defconst my/dired-audio-extensions
    '("mp3" "wav" "flac" "ogg" "aac" "m4a" "opus" "wma")
    "Known audio extensions for metadata.")

  (defconst my/dired-archive-extensions
    '("zip" "7z" "tar" "gz" "tgz" "bz2" "xz" "zst" "rar")
    "Known archive extensions for listing preview.")

  ;; External opener policy (supports remote TRAMP files via local temp copy).
  (defcustom my/external-open-policy 'ext-only
    "Policy for external opener.
`ext-only' only opens configured media/binary types externally.
`always' opens all files externally.
`never' always opens files inside Emacs."
    :type '(choice (const :tag "Configured extensions only" ext-only)
                   (const :tag "Always external" always)
                   (const :tag "Never external" never))
    :group 'files)

  (defcustom my/external-open-remote-max-bytes (* 512 1024 1024)
    "Max size for remote file copied to local temp before external open."
    :type 'integer
    :group 'files)

  (defvar my/external-file-extensions
    '("pdf" "epub"
      "wav" "mp3" "flac" "ogg" "aac" "m4a" "wma"
      "mp4" "mkv" "avi" "mov" "wmv" "webm" "flv"
      "7z" "rar"
      "docx" "doc" "xlsx" "xls" "pptx" "ppt")
    "Extensions opened externally when `my/external-open-policy' is `ext-only'.")

  (defun my/external-open--command ()
    "Return opener command for current OS."
    (cond
     ((eq system-type 'darwin) "open")
     ((eq system-type 'gnu/linux)
      (or (executable-find "xdg-open")
          (executable-find "gio")
          (user-error "No opener found: install xdg-open or gio")))
     ((eq system-type 'windows-nt) "cmd.exe")
     (t (user-error "Unsupported system-type for external open: %s" system-type))))

  (defun my/external-open--prepare-local-file (file)
    "Return local path for FILE, copying remote files to temp when needed."
    (if (file-remote-p file)
        (let* ((attrs (file-attributes file 'string))
               (size (if attrs (file-attribute-size attrs) 0)))
          (when (and (integerp size) (> size my/external-open-remote-max-bytes))
            (user-error "Remote file too large for external open (%s)"
                        (file-size-human-readable size)))
          (let* ((ext (file-name-extension file t))
                 (tmp (make-temp-file "emacs-remote-open-" nil ext)))
            (copy-file file tmp t)
            tmp))
      (expand-file-name file)))

  (defun my/external-file-p (filename)
    "Return non-nil when FILENAME should be opened externally."
    (pcase my/external-open-policy
      ('always t)
      ('never nil)
      (_ (member (downcase (or (file-name-extension filename) ""))
                 my/external-file-extensions))))

  (defun my/external-open-policy-cycle ()
    "Cycle external open policy: ext-only -> always -> never."
    (interactive)
    (setq my/external-open-policy
          (pcase my/external-open-policy
            ('ext-only 'always)
            ('always 'never)
            (_ 'ext-only)))
    (message "External opener policy: %s" my/external-open-policy))

  (defun my/open-externally (file)
    "Open FILE with system app according to opener policy."
    (interactive (list (or (dired-get-filename nil t)
                           (buffer-file-name)
                           (read-file-name "File: "))))
    (let* ((target (my/external-open--prepare-local-file file))
           (opener (my/external-open--command)))
      (cond
       ((and (eq system-type 'gnu/linux)
             (string-match-p "/gio\\'" opener))
        (start-process "open-external" nil opener "open" target))
       ((eq system-type 'windows-nt)
        (start-process "open-external" nil opener "/c" "start" "" target))
       (t
        (start-process "open-external" nil opener target)))
      (message "Opened externally via %s: %s" (file-name-nondirectory opener) target)))

  ;; Make dired `!' smart: marked files respect opener policy.
  (defun my/dired-do-open (arg)
    "Open marked (or current) files with system app. Handles remote files."
    (interactive "P")
    (let ((files (dired-get-marked-files t arg)))
      (dolist (f files)
        (my/open-externally f))))

  ;; Advise find-file to open matching files externally.
  (define-advice find-file (:around (orig-fn filename &rest args) external-open)
    (if (my/external-file-p filename)
        (my/open-externally filename)
      (apply orig-fn filename args)))

  (defun my/dired--current-file ()
    "Return current file in Dired."
    (or (dired-get-filename nil t)
        (user-error "No file on current line")))

  (defun my/dired--capture-command-output (program &rest args)
    "Run PROGRAM ARGS and return output string on success."
    (when (executable-find program)
      (with-temp-buffer
        (let ((status (apply #'process-file program nil t nil args)))
          (when (eq status 0)
            (buffer-string))))))

  (defun my/dired--trim-lines (text max-lines)
    "Trim TEXT to at most MAX-LINES."
    (if (not (stringp text))
        ""
      (string-join (seq-take (split-string text "\n" t) max-lines) "\n")))

  (defun my/dired--display-side-buffer (buffer side slot width)
    "Display BUFFER in SIDE/SLOT with WIDTH."
    (display-buffer
     buffer
     `((display-buffer-in-side-window)
       (side . ,side)
       (slot . ,slot)
       (window-width . ,width))))

  (defun my/dired--notify (fmt &rest args)
    "Show concise yazi-style feedback using FMT and ARGS."
    (let ((msg (apply #'format fmt args)))
      (message "[Dired] %s" msg)
      (when (fboundp 'pulse-momentary-highlight-one-line)
        (ignore-errors
          (pulse-momentary-highlight-one-line (point))))))

  (defun my/dired-follow-cursor ()
    "Keep point in view for more immediate cursor-follow feedback."
    (when (and my/dired-follow-cursor
               (derived-mode-p 'dired-mode)
               (not (equal my/dired--interaction-last-point (point))))
      (setq my/dired--interaction-last-point (point))
      (unless (pos-visible-in-window-p (point) (selected-window))
        (recenter))))

  (defvar my/dired--preview-cache-write-count 0)

  (defun my/dired--preview-cache-ensure ()
    "Ensure preview cache directories exist."
    (make-directory my/dired-preview-cache-directory t)
    (make-directory (file-name-directory my/dired-preview-hash-index-file) t))

  (defun my/dired-preview-hash-index-load ()
    "Load file signature -> content hash index."
    (setq my/dired-preview-hash-index (make-hash-table :test 'equal))
    (when (file-exists-p my/dired-preview-hash-index-file)
      (let ((data (ignore-errors
                    (with-temp-buffer
                      (insert-file-contents my/dired-preview-hash-index-file)
                      (read (current-buffer))))))
        (dolist (it data)
          (when (and (consp it) (stringp (car it)) (stringp (cdr it)))
            (puthash (car it) (cdr it) my/dired-preview-hash-index))))))

  (defun my/dired-preview-hash-index-save ()
    "Persist file signature -> content hash index."
    (my/dired--preview-cache-ensure)
    (with-temp-file my/dired-preview-hash-index-file
      (let (acc)
        (maphash (lambda (k v) (push (cons k v) acc)) my/dired-preview-hash-index)
        (prin1 acc (current-buffer)))))

  (defun my/dired--preview-signature (file)
    "Build stable signature from FILE path, size and mtime."
    (let* ((abs (expand-file-name file))
           (attrs (ignore-errors (file-attributes abs 'string)))
           (size (and attrs (file-attribute-size attrs)))
           (mtime (and attrs (file-attribute-modification-time attrs))))
      (when attrs
        (format "%s|%s|%.0f"
                abs
                (if (integerp size) size 0)
                (if mtime (float-time mtime) 0.0)))))

  (defun my/dired--content-hash (origin-file local-file)
    "Return cached content hash for ORIGIN-FILE, or compute from LOCAL-FILE."
    (let* ((sig (my/dired--preview-signature origin-file))
           (known (and sig (gethash sig my/dired-preview-hash-index)))
           (hash (or known
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally (expand-file-name local-file))
                       (secure-hash 'sha256 (current-buffer))))))
      (when (and sig (not known))
        (puthash sig hash my/dired-preview-hash-index)
        (my/dired-preview-hash-index-save))
      hash))

  (defun my/dired--preview-cache-ext (kind)
    "Return cache file extension for KIND."
    (pcase kind
      ('thumb "bin")
      ('meta "txt")
      (_ "cache")))

  (defun my/dired--preview-cache-path (kind hash)
    "Build preview cache file path for KIND and HASH."
    (expand-file-name
     (format "%s-%s.%s"
             (if (symbolp kind) (symbol-name kind) kind)
             hash
             (my/dired--preview-cache-ext kind))
     my/dired-preview-cache-directory))

  (defun my/dired--preview-cache-valid-p (cache-file)
    "Return non-nil when CACHE-FILE exists and is within TTL."
    (when (file-exists-p cache-file)
      (let* ((attrs (file-attributes cache-file 'string))
             (mtime (and attrs (file-attribute-modification-time attrs))))
        (if (and mtime
                 (> (- (float-time) (float-time mtime))
                    my/dired-preview-cache-ttl-seconds))
            (progn
              (ignore-errors (delete-file cache-file))
              nil)
          t))))

  (defun my/dired--preview-cache-read-binary (cache-file)
    "Read CACHE-FILE as unibyte string."
    (when (my/dired--preview-cache-valid-p cache-file)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally cache-file)
        (buffer-string))))

  (defun my/dired--preview-cache-read-text (cache-file)
    "Read CACHE-FILE as text."
    (when (my/dired--preview-cache-valid-p cache-file)
      (with-temp-buffer
        (insert-file-contents cache-file)
        (buffer-string))))

  (defun my/dired--preview-cache-write-binary (cache-file data)
    "Write unibyte DATA into CACHE-FILE."
    (my/dired--preview-cache-ensure)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert data)
      (write-region (point-min) (point-max) cache-file nil 'silent)))

  (defun my/dired--preview-cache-write-text (cache-file text)
    "Write TEXT into CACHE-FILE."
    (my/dired--preview-cache-ensure)
    (with-temp-file cache-file
      (insert text)))

  (defun my/dired-preview-cache-prune ()
    "Prune stale preview cache files and compact hash index."
    (interactive)
    (my/dired--preview-cache-ensure)
    (let* ((now (float-time))
           (files (directory-files my/dired-preview-cache-directory t "^[^.].*"))
           kept)
      (dolist (f files)
        (let* ((attrs (file-attributes f 'string))
               (mtime (and attrs (file-attribute-modification-time attrs)))
               (age (if mtime (- now (float-time mtime)) most-positive-fixnum)))
          (if (> age my/dired-preview-cache-ttl-seconds)
              (ignore-errors (delete-file f))
            (push (cons f (if mtime (float-time mtime) 0.0)) kept))))
      (setq kept (sort kept (lambda (a b) (> (cdr a) (cdr b)))))
      (dolist (pair (nthcdr my/dired-preview-cache-max-entries kept))
        (ignore-errors (delete-file (car pair)))))
    (let ((new-index (make-hash-table :test 'equal)))
      (maphash
       (lambda (sig hash)
         (when (or (my/dired--preview-cache-valid-p (my/dired--preview-cache-path 'thumb hash))
                   (my/dired--preview-cache-valid-p (my/dired--preview-cache-path 'meta hash)))
           (puthash sig hash new-index)))
       my/dired-preview-hash-index)
      (setq my/dired-preview-hash-index new-index)
      (my/dired-preview-hash-index-save)))

  (defun my/dired-preview-cache-clear ()
    "Clear all preview cache files and hash index."
    (interactive)
    (when (file-directory-p my/dired-preview-cache-directory)
      (dolist (f (directory-files my/dired-preview-cache-directory t "^[^.].*"))
        (ignore-errors (delete-file f))))
    (setq my/dired-preview-hash-index (make-hash-table :test 'equal))
    (my/dired-preview-hash-index-save)
    (message "Preview cache cleared"))

  (defun my/dired--preview-cache-maybe-prune ()
    "Prune cache occasionally after writes."
    (setq my/dired--preview-cache-write-count (1+ my/dired--preview-cache-write-count))
    (when (>= my/dired--preview-cache-write-count 20)
      (setq my/dired--preview-cache-write-count 0)
      (my/dired-preview-cache-prune)))

  (defun my/dired--preview-cache-get (kind file)
    "Return cached preview payload for KIND and FILE, if available."
    (let* ((sig (my/dired--preview-signature file))
           (hash (and sig (gethash sig my/dired-preview-hash-index)))
           (cache-file (and hash (my/dired--preview-cache-path kind hash)))
           (payload (pcase kind
                      ('thumb (and cache-file (my/dired--preview-cache-read-binary cache-file)))
                      ('meta (and cache-file (my/dired--preview-cache-read-text cache-file)))
                      (_ nil))))
      (when (and sig hash (not payload))
        (remhash sig my/dired-preview-hash-index)
        (my/dired-preview-hash-index-save))
      payload))

  (defun my/dired--preview-cache-put (kind origin-file local-file payload)
    "Cache PAYLOAD for KIND using ORIGIN-FILE identity and LOCAL-FILE content."
    (let* ((hash (my/dired--content-hash origin-file local-file))
           (cache-file (my/dired--preview-cache-path kind hash)))
      (pcase kind
        ('thumb (my/dired--preview-cache-write-binary cache-file payload))
        ('meta (my/dired--preview-cache-write-text cache-file payload)))
      (my/dired--preview-cache-maybe-prune)
      payload))

  (defun my/dired--with-local-preview-file (file fn)
    "Call FN with a local copy of FILE when needed."
    (if (not (file-remote-p file))
        (funcall fn (expand-file-name file) nil)
      (let* ((attrs (file-attributes file 'string))
             (size (if attrs (file-attribute-size attrs) 0)))
        (when (and (integerp size) (> size my/dired-remote-preview-max-bytes))
          (user-error "Remote file too large for preview (%s)"
                      (file-size-human-readable size)))
        (let* ((ext (file-name-extension file t))
               (tmp (make-temp-file "emacs-remote-preview-" nil ext)))
          (copy-file file tmp t)
          (unwind-protect
              (funcall fn tmp t)
            (ignore-errors (delete-file tmp)))))))

  (defun my/dired--image-data (file)
    "Return FILE content as unibyte string for `create-image'."
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (buffer-string)))

  (defun my/dired--generate-video-thumbnail (file)
    "Return image data thumbnail generated from video FILE."
    (let ((thumb (make-temp-file "dired-video-thumb-" nil ".jpg")))
      (unwind-protect
          (let ((ok (or (and (executable-find "ffmpegthumbnailer")
                             (eq 0 (process-file "ffmpegthumbnailer" nil nil nil
                                                 "-i" file "-o" thumb "-s" "640")))
                        (and (executable-find "ffmpeg")
                             (eq 0 (process-file "ffmpeg" nil nil nil
                                                 "-y" "-loglevel" "error"
                                                 "-ss" "00:00:01"
                                                 "-i" file "-frames:v" "1" thumb))))))
            (when (and ok (file-exists-p thumb))
              (my/dired--image-data thumb)))
        (ignore-errors (delete-file thumb)))))

  (defun my/dired--generate-pdf-thumbnail (file)
    "Return image data thumbnail generated from pdf FILE."
    (let* ((base (make-temp-file "dired-pdf-thumb-"))
           (thumb (concat base ".jpg")))
      (unwind-protect
          (when (and (executable-find "pdftoppm")
                     (eq 0 (process-file "pdftoppm" nil nil nil
                                         "-f" "1" "-singlefile" "-jpeg" file base))
                     (file-exists-p thumb))
            (my/dired--image-data thumb))
        (ignore-errors (delete-file base))
        (ignore-errors (delete-file thumb)))))

  (defun my/dired--render-thumbnail-buffer (file data source-label)
    "Render thumbnail DATA of FILE with SOURCE-LABEL in side buffer."
    (with-current-buffer (get-buffer-create my/dired-thumbnail-buffer-name)
      (let ((inhibit-read-only t)
            (img (ignore-errors
                   (create-image data nil t :max-width 560 :max-height 420))))
        (erase-buffer)
        (insert (format "Thumbnail: %s (%s)\n\n"
                        (abbreviate-file-name file)
                        source-label))
        (if img
            (insert-image img)
          (insert "(unable to render thumbnail)"))
        (goto-char (point-min))
        (special-mode)))
    (my/dired--display-side-buffer
     (get-buffer my/dired-thumbnail-buffer-name) 'right 1 0.35))

  (defun my/dired-preview-thumbnail ()
    "Preview image/video/pdf thumbnail for current file in side window."
    (interactive)
    (let* ((file (my/dired--current-file))
           (cached (my/dired--preview-cache-get 'thumb file)))
      (when (file-directory-p file)
        (user-error "Directory has no thumbnail preview"))
      (if cached
          (my/dired--render-thumbnail-buffer
           file cached (if (file-remote-p file) "cache/remote" "cache"))
        (my/dired--with-local-preview-file
         file
         (lambda (local remote-p)
           (let* ((ext (downcase (or (file-name-extension local) "")))
                  (data
                   (cond
                    ((image-type-from-file-name local) (my/dired--image-data local))
                    ((member ext my/dired-video-extensions)
                     (my/dired--generate-video-thumbnail local))
                    ((string= ext "pdf")
                     (my/dired--generate-pdf-thumbnail local))
                    (t nil))))
             (unless data
               (user-error "No thumbnail backend available for this file"))
             (my/dired--preview-cache-put 'thumb file local data)
             (my/dired--render-thumbnail-buffer
              file data (if remote-p "remote+cache" "local+cache")))))))

  (defun my/dired--archive-list-output (file ext)
    "Return archive listing text for FILE with extension EXT."
    (or (and (string= ext "zip")
             (my/dired--capture-command-output "unzip" "-l" file))
        (my/dired--capture-command-output "7zz" "l" file)
        (my/dired--capture-command-output "7z" "l" file)
        (and (member ext my/dired-archive-extensions)
             (my/dired--capture-command-output "tar" "-tf" file))))

  (defun my/dired--metadata-body (local-file)
    "Build expensive metadata body for LOCAL-FILE."
    (let* ((ext (downcase (or (file-name-extension local-file) "")))
           (file-type (my/dired--capture-command-output "file" "-b" local-file))
           (exif (my/dired--capture-command-output "exiftool" "-s" "-G1" local-file))
           (media (my/dired--capture-command-output "mediainfo" "--Output=JSON" local-file))
           (pdf (and (string= ext "pdf")
                     (my/dired--capture-command-output "pdfinfo" local-file)))
           (arch (and (member ext my/dired-archive-extensions)
                      (my/dired--archive-list-output local-file ext))))
      (with-temp-buffer
        (when file-type
          (insert "Type:\n")
          (insert (my/dired--trim-lines file-type 8) "\n\n"))
        (when exif
          (insert "Exiftool:\n")
          (insert (my/dired--trim-lines exif 80) "\n\n"))
        (when media
          (insert "MediaInfo(JSON):\n")
          (insert (my/dired--trim-lines media 120) "\n\n"))
        (when pdf
          (insert "PDFInfo:\n")
          (insert (my/dired--trim-lines pdf 80) "\n\n"))
        (when arch
          (insert "Archive Entries:\n")
          (insert (my/dired--trim-lines arch 120) "\n\n"))
        (buffer-string))))

  (defun my/dired--render-metadata-buffer (file size mtime body source-label)
    "Render metadata preview for FILE with BODY and SOURCE-LABEL."
    (with-current-buffer (get-buffer-create my/dired-metadata-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Metadata: %s (%s)\n\n"
                        (abbreviate-file-name file)
                        source-label))
        (insert (format "Size: %s\n" (if (integerp size)
                                         (file-size-human-readable size)
                                       "N/A")))
        (insert (format "Modified: %s\n\n"
                        (if mtime
                            (format-time-string "%Y-%m-%d %H:%M:%S %z" mtime)
                          "N/A")))
        (insert (or body ""))
        (goto-char (point-min))
        (special-mode)))
    (my/dired--display-side-buffer
     (get-buffer my/dired-metadata-buffer-name) 'right 2 0.42))

  (defun my/dired-show-metadata ()
    "Show detailed metadata for current file, including media/archive info."
    (interactive)
    (let* ((file (my/dired--current-file))
           (attrs (file-attributes file 'string))
           (size (if attrs (file-attribute-size attrs) 0))
           (mtime (if attrs (file-attribute-modification-time attrs) nil))
           (cached (my/dired--preview-cache-get 'meta file)))
      (if cached
          (my/dired--render-metadata-buffer
           file size mtime cached (if (file-remote-p file) "cache/remote" "cache"))
        (my/dired--with-local-preview-file
         file
         (lambda (local remote-p)
           (let ((body (my/dired--metadata-body local)))
             (my/dired--preview-cache-put 'meta file local body)
             (my/dired--render-metadata-buffer
              file size mtime body (if remote-p "remote+cache" "local+cache"))))))))

  (defconst my/dired-sort-cycle
    `((name . ,(my/dired--listing-switches 'name))
      (mtime . ,(my/dired--listing-switches 'mtime))
      (size . ,(my/dired--listing-switches 'size)))
    "Sort presets for `my/dired-cycle-sort'.")

  (defvar-local my/dired-sort-cycle-state 'name
    "Current sort state used by `my/dired-cycle-sort'.")

  (defun my/dired-toggle-hidden ()
    "Toggle hidden file visibility in Dired."
    (interactive)
    (unless (derived-mode-p 'dired-mode)
      (user-error "Not in a Dired buffer"))
    (require 'dired-x)
    (let ((hide-hidden (not (bound-and-true-p dired-omit-mode))))
      (dired-omit-mode (if hide-hidden 1 -1))
      (revert-buffer)
      (my/dired--notify "Hidden files: %s" (if hide-hidden "hidden" "shown"))))

  (defun my/dired-cycle-sort ()
    "Cycle Dired sort like yazi: name -> time -> size."
    (interactive)
    (setq my/dired-sort-cycle-state
          (pcase my/dired-sort-cycle-state
            ('name 'mtime)
            ('mtime 'size)
            (_ 'name)))
    (setq-local dired-listing-switches
                (alist-get my/dired-sort-cycle-state my/dired-sort-cycle))
    (revert-buffer)
    (my/dired--notify
     "Sort: %s (%d entries)"
     (symbol-name my/dired-sort-cycle-state)
     (count-lines (point-min) (point-max))))

  (defun my/dired--ranger-current-files ()
    "Return current fileset from `dired-ranger-copy-ring'."
    (when (and (require 'dired-ranger nil t)
               (boundp 'dired-ranger-copy-ring)
               (not (ring-empty-p dired-ranger-copy-ring)))
      (cdr (ring-ref dired-ranger-copy-ring 0))))

  (defun my/dired-yank-files ()
    "Copy marked/current files to a Dired paste ring (yazi-like yank)."
    (interactive)
    (unless (require 'dired-ranger nil t)
      (user-error "dired-ranger not available"))
    (let ((count (length (dired-get-marked-files))))
      (call-interactively #'dired-ranger-copy)
      (my/dired-operation-queue-refresh-open)
      (my/dired--notify "Yanked %d item(s)" count)))

  (defun my/dired-cut-files ()
    "Cut marked/current files to a Dired paste ring (yazi-like cut)."
    (interactive)
    (unless (require 'dired-ranger nil t)
      (user-error "dired-ranger not available"))
    (let ((count (length (dired-get-marked-files))))
      (call-interactively #'dired-ranger-move)
      (my/dired-operation-queue-refresh-open)
      (my/dired--notify "Cut %d item(s)" count)))

  (defun my/dired-paste-files ()
    "Paste files with conflict summary prompt before actual paste."
    (interactive)
    (unless (require 'dired-ranger nil t)
      (user-error "dired-ranger not available"))
    (let* ((files (my/dired--ranger-current-files))
           (conflicts
            (cl-loop for src in files
                     for dst = (expand-file-name (file-name-nondirectory src)
                                                 default-directory)
                     when (file-exists-p dst)
                     collect dst)))
      (unless files
        (user-error "Clipboard ring is empty"))
      (when conflicts
        (unless (y-or-n-p
                 (format "Detected %d conflict(s). Continue and resolve interactively? "
                         (length conflicts)))
          (user-error "Paste canceled")))
      (call-interactively #'dired-ranger-paste)
      (my/dired-operation-queue-refresh-open)
      (my/dired--notify "Pasted %d item(s)%s"
                        (length files)
                        (if conflicts
                            (format ", conflicts: %d" (length conflicts))
                          ""))))

  (defun my/dired-copy-path (&optional relative)
    "Copy current file path. With RELATIVE, copy path relative to Dired cwd."
    (interactive "P")
    (let* ((file (or (dired-get-filename nil t)
                     (user-error "No file on current line")))
           (path (if relative
                     (file-relative-name file default-directory)
                   (expand-file-name file))))
      (kill-new path)
      (message "Copied path: %s" path)))

  (defun my/dired-bulk-rename ()
    "Enter writable Dired mode for batch rename/edit."
    (interactive)
    (unless (derived-mode-p 'dired-mode)
      (user-error "Not in a Dired buffer"))
    (wdired-change-to-wdired-mode))

  (defun my/dired-yazi-session ()
    "Open a yazi-like two-pane file manager session."
    (interactive)
    (tc/dired-two-panes)
    (when (fboundp 'dirvish-layout-toggle)
      (ignore-errors (dirvish-layout-toggle))))

  (defun my/dired-session-load ()
    "Load persisted Dired session directories."
    (when (file-exists-p my/dired-session-file)
      (setq my/dired-session-dirs
            (with-temp-buffer
              (insert-file-contents my/dired-session-file)
              (read (current-buffer))))))

  (defun my/dired-session-save ()
    "Save current Dired session directories."
    (make-directory (file-name-directory my/dired-session-file) t)
    (with-temp-file my/dired-session-file
      (let ((print-length nil)
            (print-level nil))
        (prin1 my/dired-session-dirs (current-buffer)))))

  (defun my/dired-session-record ()
    "Record current Dired directory for session restore."
    (when (derived-mode-p 'dired-mode)
      (setq my/dired-session-dirs
            (seq-take
             (cons (file-name-as-directory (expand-file-name default-directory))
                   (delete (file-name-as-directory (expand-file-name default-directory))
                           my/dired-session-dirs))
             my/dired-session-max-count))
      (my/dired-session-save)))

  (defun my/dired-session-restore (&optional all)
    "Restore previous Dired/Dirvish session.  With ALL, restore all entries."
    (interactive "P")
    (my/dired-session-load)
    (unless my/dired-session-dirs
      (user-error "No saved Dired session"))
    (if all
        (progn
          (delete-other-windows)
          (dolist (dir (reverse my/dired-session-dirs))
            (tab-new)
            (if (fboundp 'dirvish) (dirvish dir) (dired dir)))
          (message "Restored %d directories in tabs" (length my/dired-session-dirs)))
      (if (fboundp 'dirvish)
          (dirvish (car my/dired-session-dirs))
        (dired (car my/dired-session-dirs)))))

  (defun my/dired-tags-load ()
    "Load persisted file tags."
    (setq my/dired-tags-table (make-hash-table :test 'equal))
    (when (file-exists-p my/dired-tags-file)
      (dolist (item (with-temp-buffer
                      (insert-file-contents my/dired-tags-file)
                      (read (current-buffer))))
        (puthash (car item) (cdr item) my/dired-tags-table))))

  (defun my/dired-tags-save ()
    "Persist file tags."
    (make-directory (file-name-directory my/dired-tags-file) t)
    (with-temp-file my/dired-tags-file
      (let (acc)
        (maphash (lambda (k v) (push (cons k v) acc)) my/dired-tags-table)
        (prin1 acc (current-buffer)))))

  (defun my/dired-tag-add (tag)
    "Add TAG to current file."
    (interactive "sTag: ")
    (let* ((file (my/dired--current-file))
           (key (expand-file-name file))
           (tags (delete-dups (cons tag (copy-sequence (gethash key my/dired-tags-table))))))
      (puthash key tags my/dired-tags-table)
      (my/dired-tags-save)
      (message "Tags for %s: %s"
               (file-name-nondirectory key)
               (string-join tags ", "))))

  (defun my/dired-tag-remove (tag)
    "Remove TAG from current file."
    (interactive
     (let* ((file (my/dired--current-file))
            (tags (gethash (expand-file-name file) my/dired-tags-table)))
       (list (completing-read "Remove tag: " tags nil t))))
    (let* ((file (expand-file-name (my/dired--current-file)))
           (tags (delete tag (copy-sequence (gethash file my/dired-tags-table)))))
      (if tags
          (puthash file tags my/dired-tags-table)
        (remhash file my/dired-tags-table))
      (my/dired-tags-save)
      (message "Tag removed: %s" tag)))

  (defun my/dired-tag-open (tag)
    "Open virtual Dired buffer for files tagged with TAG."
    (interactive
     (list
      (let (all)
        (maphash (lambda (_k v) (setq all (append v all))) my/dired-tags-table)
        (completing-read "Tag: " (delete-dups all) nil t))))
    (let (files)
      (maphash (lambda (k v)
                 (when (and (member tag v) (file-exists-p k))
                   (push k files)))
               my/dired-tags-table)
      (if files
          (dired (cons (format "*tag:%s*" tag) (nreverse files)))
        (message "No files for tag: %s" tag))))

  (defun my/dired-bookmark-add ()
    "Bookmark current location."
    (interactive)
    (call-interactively #'bookmark-set))

  (defun my/dired-bookmark-jump ()
    "Jump to bookmark."
    (interactive)
    (call-interactively #'bookmark-jump))

  (defun my/dired--percent-number (value)
    "Parse percent VALUE into integer 0..100."
    (cond
     ((numberp value) (max 0 (min 100 (round value))))
     ((stringp value)
      (let ((digits (replace-regexp-in-string "[^0-9]" "" value)))
        (if (string-empty-p digits) nil
          (max 0 (min 100 (string-to-number digits))))))
     (t nil)))

  (defun my/dired--progress-bar (percent &optional width)
    "Build ASCII progress bar from PERCENT and WIDTH."
    (let* ((width (or width 24))
           (p (max 0 (min 100 (or percent 0))))
           (filled (floor (* width p) 100)))
      (format "[%s%s] %3d%%"
              (make-string filled ?#)
              (make-string (- width filled) ?-)
              p)))

  (defun my/dired--yank-task-info (buf)
    "Return plist describing async yank task in BUF."
    (when (buffer-live-p buf)
      (let* ((proc (get-buffer-process buf))
             (status (if proc (process-status proc) 'exit))
             (details (and (fboundp 'dirvish-prop)
                           (with-current-buffer buf
                             (ignore-errors (dirvish-prop :yank-details)))))
             (percent-raw (and (fboundp 'dirvish-prop)
                               (with-current-buffer buf
                                 (ignore-errors (dirvish-prop :yank-percent)))))
             (percent (my/dired--percent-number percent-raw))
             (srcs (and (consp details) (nth 1 details)))
             (dest (and (consp details) (nth 2 details)))
             (method (and (consp details) (nth 3 details)))
             (method-name
              (cond
               ((and method (boundp 'dirvish-yank-fn-string))
                (format "%s" (or (alist-get method dirvish-yank-fn-string) method)))
               (method (format "%s" method))
               (t "task"))))
        (list :buffer buf
              :status status
              :percent percent
              :method method-name
              :count (if (listp srcs) (length srcs) 0)
              :dest (if (stringp dest) dest "")))))

  (defun my/dired-operation-queue-refresh (&optional buffer force)
    "Refresh operation queue BUFFER content.
With FORCE, refresh even when BUFFER is not visible."
    (interactive)
    (let ((buffer (or buffer (get-buffer my/dired-operation-queue-buffer-name))))
      (when (and (buffer-live-p buffer)
                 (or force (get-buffer-window buffer t)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t)
                (saved-point (point)))
            (erase-buffer)
            (insert "Dired/Dirvish Operation Queue (Realtime)\n")
            (insert (format "Updated: %s\n\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S")))
            (insert "Clipboard Ring (dired-ranger):\n")
            (if (and (boundp 'dired-ranger-copy-ring)
                     (not (ring-empty-p dired-ranger-copy-ring)))
                (dotimes (i (ring-length dired-ranger-copy-ring))
                  (let* ((entry (ring-ref dired-ranger-copy-ring i))
                         (files (cdr entry)))
                    (insert (format "  [%d] %d item(s)\n" i (length files)))
                    (dolist (f (seq-take files 6))
                      (insert (format "      - %s\n" (abbreviate-file-name f))))))
              (insert "  (empty)\n"))
            (insert "\nAsync Jobs (dirvish-yank):\n")
            (let ((tasks (and (boundp 'dirvish-yank-log-buffers)
                              (mapcar #'my/dired--yank-task-info dirvish-yank-log-buffers))))
              (setq tasks (delq nil tasks))
              (if tasks
                  (let* ((running (cl-count-if (lambda (it)
                                                 (memq (plist-get it :status) '(run open)))
                                               tasks))
                         (percents (delq nil (mapcar (lambda (it) (plist-get it :percent)) tasks)))
                         (avg (if percents
                                  (/ (apply #'+ percents) (max 1 (length percents)))
                                nil)))
                    (insert (format "  tasks=%d running=%d\n" (length tasks) running))
                    (when avg
                      (insert (format "  overall %s\n" (my/dired--progress-bar avg 28))))
                    (insert "\n")
                    (dolist (it tasks)
                      (let* ((status (plist-get it :status))
                             (pct (plist-get it :percent))
                             (bar (if pct
                                      (my/dired--progress-bar pct 22)
                                    "[----------------------]  ???%"))
                             (method (plist-get it :method))
                             (count (plist-get it :count))
                             (dest (plist-get it :dest))
                             (buf (plist-get it :buffer)))
                        (insert (format "  %-8s %s (%s files, %s)\n"
                                        method bar count status))
                        (when (and dest (not (string-empty-p dest)))
                          (insert (format "      -> %s\n" (abbreviate-file-name dest))))
                        (insert-text-button
                         (format "      log: %s\n" (buffer-name buf))
                         'action (lambda (_)
                                   (interactive)
                                   (pop-to-buffer buf))
                         'follow-link t))))
                (insert "  (no async jobs)\n")))
            (goto-char (min saved-point (point-max))))))))

  (defun my/dired-operation-queue--stop-timer ()
    "Stop operation queue realtime timer in current buffer."
    (when (timerp my/dired-operation-queue--timer)
      (cancel-timer my/dired-operation-queue--timer)
      (setq my/dired-operation-queue--timer nil)))

  (defun my/dired-operation-queue--start-timer ()
    "Start operation queue realtime timer in current buffer."
    (my/dired-operation-queue--stop-timer)
    (setq my/dired-operation-queue--timer
          (run-with-timer
           0 my/dired-operation-queue-refresh-interval
           #'my/dired-operation-queue-refresh
           (current-buffer))))

  (define-derived-mode my/dired-operation-queue-mode special-mode "Dired-Queue"
    "Major mode for realtime Dired operation queue."
    (setq-local truncate-lines t)
    (my/dired-operation-queue--start-timer)
    (add-hook 'kill-buffer-hook #'my/dired-operation-queue--stop-timer nil t))
  (define-key my/dired-operation-queue-mode-map (kbd "g") #'my/dired-operation-queue-refresh)

  (defun my/dired-operation-queue-refresh-open ()
    "Refresh queue buffer when it is visible."
    (let ((buf (get-buffer my/dired-operation-queue-buffer-name)))
      (when (and buf (get-buffer-window buf))
        (my/dired-operation-queue-refresh buf t))))

  (defun my/dired-operation-queue ()
    "Show operation queue with realtime refresh and progress bars."
    (interactive)
    (let ((buf (get-buffer-create my/dired-operation-queue-buffer-name)))
      (with-current-buffer buf
        (unless (derived-mode-p 'my/dired-operation-queue-mode)
          (my/dired-operation-queue-mode))
        (my/dired-operation-queue-refresh buf t))
      (pop-to-buffer buf)))

  (defun my/dired-live-preview--run (buffer)
    "Update live preview for BUFFER."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and my/dired-live-preview-mode (derived-mode-p 'dired-mode))
          (let ((file (dired-get-filename nil t)))
            (when (and file (not (equal file my/dired--live-preview-last-file)))
              (setq my/dired--live-preview-last-file file)
              (when (get-buffer-window my/dired-thumbnail-buffer-name)
                (ignore-errors (my/dired-preview-thumbnail)))
              (when (get-buffer-window my/dired-metadata-buffer-name)
                (ignore-errors (my/dired-show-metadata)))))))))

  (defun my/dired-live-preview--schedule ()
    "Schedule idle live preview refresh."
    (when my/dired-live-preview-mode
      (when (timerp my/dired--live-preview-timer)
        (cancel-timer my/dired--live-preview-timer))
      (setq my/dired--live-preview-timer
            (run-with-idle-timer my/dired-live-preview-idle-delay nil #'my/dired-live-preview--run
                                 (current-buffer)))))

  (define-minor-mode my/dired-live-preview-mode
    "Live-update metadata/thumbnail windows when cursor moves."
    :lighter " LivePv"
    (if my/dired-live-preview-mode
        (add-hook 'post-command-hook #'my/dired-live-preview--schedule nil t)
      (remove-hook 'post-command-hook #'my/dired-live-preview--schedule t)
      (when (timerp my/dired--live-preview-timer)
        (cancel-timer my/dired--live-preview-timer)
        (setq my/dired--live-preview-timer nil))))

  (transient-define-prefix my/file-manager-panel ()
    "Unified command panel for file manager workflows."
    [["Preview"
     ("v" "Toggle quick preview" tc/dired-toggle-preview)
      ("t" "Thumbnail preview" my/dired-preview-thumbnail)
      ("i" "Metadata preview" my/dired-show-metadata)
      ("l" "Toggle live preview" my/dired-live-preview-mode)
      ("c" "Prune preview cache" my/dired-preview-cache-prune)
      ("C" "Clear preview cache" my/dired-preview-cache-clear)
      ("o" "Open externally" my/open-externally)]
     ["Workflow"
      ("y" "Yank" my/dired-yank-files)
      ("x" "Cut" my/dired-cut-files)
      ("p" "Paste" my/dired-paste-files)
      ("r" "Bulk rename" my/dired-bulk-rename)
      (";" "Dirvish dispatch" dirvish-dispatch)
      ("O" "Cycle opener policy" my/external-open-policy-cycle)]
     ["State"
      ("s" "Save session" my/dired-session-save)
      ("S" "Restore session" my/dired-session-restore)
      ("q" "Operation queue" my/dired-operation-queue)
      ("b" "Bookmark add" my/dired-bookmark-add)
      ("j" "Bookmark jump" my/dired-bookmark-jump)
      ("a" "Tag add" my/dired-tag-add)
      ("A" "Tag remove" my/dired-tag-remove)
      ("g" "Open by tag" my/dired-tag-open)]])

  (defun my/dired-interaction-setup ()
    "Apply interaction details for yazi-like responsiveness."
    (setq-local scroll-margin 2
                scroll-conservatively 101
                auto-hscroll-mode nil
                mouse-wheel-progressive-speed nil)
    (setq-local my/dired-sort-cycle-state 'name)
    (add-hook 'post-command-hook #'my/dired-follow-cursor nil t)
    (my/dired-live-preview-mode 1))

  (add-hook 'dired-mode-hook #'my/dired-interaction-setup)
  (add-hook 'dired-mode-hook #'my/dired-session-record)
  (add-hook 'kill-emacs-hook #'my/dired-session-save)

  (my/dired-session-load)
  (my/dired-tags-load)
  (my/dired-preview-hash-index-load)
  (run-with-idle-timer 3 nil #'my/dired-preview-cache-prune)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (and my/dired-session-auto-restore my/dired-session-dirs)
                (run-with-idle-timer 1.0 nil
                                     (lambda ()
                                       (when (fboundp 'dirvish)
                                         (ignore-errors
                                           (dirvish (car my/dired-session-dirs)))))))))

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
      (define-key map (kbd "C-c ;") #'my/file-manager-panel)
      (define-key map (kbd "C-c ,") #'dirvish-dispatch)
      (define-key map (kbd "C-c a") #'dirvish-quick-access)
      (define-key map (kbd "C-c f") #'dirvish-fd)
      (define-key map (kbd "C-c t") #'dirvish-subtree-toggle)
      (define-key map (kbd "C-c E") #'dirvish-emerge-menu)
      (define-key map (kbd "C-c i") #'my/dired-show-metadata)
      (define-key map (kbd "C-c T") #'my/dired-preview-thumbnail)
      (define-key map (kbd "C-c q") #'my/dired-operation-queue)
      (define-key map (kbd "C-c k") #'my/dired-bookmark-add)
      (define-key map (kbd "C-c K") #'my/dired-bookmark-jump)
      (define-key map (kbd "C-c z") #'my/dired-tag-open)
      (define-key map (kbd "C-c A") #'my/dired-tag-add)
      (define-key map (kbd "C-c R") #'my/dired-tag-remove)
      (define-key map (kbd "C-c S") #'my/dired-session-restore)
      (define-key map (kbd "C-c H") #'my/dired-toggle-hidden)
      (define-key map (kbd "C-c r") #'my/dired-cycle-sort)
      (define-key map (kbd "C-c y") #'my/dired-yank-files)
      (define-key map (kbd "C-c X") #'my/dired-cut-files)
      (define-key map (kbd "C-c P") #'my/dired-paste-files)
      (define-key map (kbd "C-c O") #'my/external-open-policy-cycle)
      (define-key map (kbd "C-c W") #'my/dired-bulk-rename)
      (define-key map (kbd "C-c Y") #'my/dired-copy-path)
      (define-key map (kbd "C-c m") #'my/dired-yazi-session)
      (define-key map (kbd "V") #'tc/dired-toggle-preview)
      (define-key map (kbd "!") #'my/dired-do-open)
      (define-key map (kbd ")") #'dired-git-info-mode)
      (define-key map (kbd "RET") #'dired-find-file)
      (define-key map (kbd "C") #'dired-do-copy)
      (define-key map (kbd "R") #'dired-do-rename)
      (define-key map (kbd "+") #'dired-create-directory)
      (define-key map (kbd "D") #'dired-do-delete)
      (define-key map (kbd "TAB") #'other-window))))
  )

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
  (use-package eat
    :commands (eat eat-other-window eat-project)
    :init
    (setq eat-kill-buffer-on-exit t
          ;; Force a conservative TERM for child shells in Eat.
          eat-term-name "xterm-256color")
    :config
    (defun my/eat-mode-setup ()
      "Disable distracting visual indicators in terminal buffers."
      (display-fill-column-indicator-mode -1)
      (display-line-numbers-mode -1)
      (setq-local global-hl-line-mode nil))
    (add-hook 'eat-mode-hook #'my/eat-mode-setup)

    (defun my/eat-clipboard-yank ()
      "Paste system clipboard into Eat."
      (interactive)
      (let ((text (cond
                   ((fboundp 'simpleclip-get-contents) (simpleclip-get-contents))
                   ((fboundp 'gui-get-selection) (gui-get-selection 'CLIPBOARD))
                   (t nil))))
        (when (and text (not (string-empty-p text)))
          (kill-new text)
          (if (fboundp 'eat-yank)
              (eat-yank)
            (yank)))))))

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

(defun my/remote-dired--open (method host &optional path)
  "Open remote HOST via TRAMP METHOD at PATH (defaults to home)."
  (let ((dir (format "/%s:%s:%s" method host (or path "~/"))))
    (if (fboundp 'dirvish)
        (dirvish dir)
      (dired dir))))

(defun my/remote-dired (host &optional path)
  "Open remote HOST with TRAMP sshx."
  (interactive
   (list (completing-read "SSH Host: " (my/ssh-hosts) nil nil)))
  (my/remote-dired--open "sshx" host path))

(global-set-key (kbd "C-c h") #'my/remote-dired)

;; ============================================================================
;; 7. GIT INTEGRATION (MAGIT)
;; ============================================================================

(my/load-feature "git"
  (require 'transient)
  (require 'seq)

  (use-package magit
    :commands (magit-status magit-file-dispatch)
    :init
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

  (defvar my/magit-repository-history nil
    "History for `my/magit-open-repository'.")

  (defun my/git-repository-roots ()
    "Return configured repository roots with scan depth.
Each element is either DIR or (DIR . DEPTH)."
    (let ((roots (and (boundp 'my/git-repository-directories)
                      my/git-repository-directories)))
      (or roots (list (cons (expand-file-name "~/Projects/") 2)))))

  (defun my/git--scan-repos-fd (root depth)
    "Discover repositories under ROOT using fd with DEPTH."
    (when (executable-find "fd")
      (ignore-errors
        (process-lines "fd"
                       "--hidden" "--follow"
                       "--type" "d"
                       "--max-depth" (number-to-string (max 1 depth))
                       "^\\.git$"
                       root))))

  (defun my/git--scan-repos-find (root depth)
    "Discover repositories under ROOT using find with DEPTH."
    (when (executable-find "find")
      (ignore-errors
        (process-lines "find"
                       root
                       "-maxdepth" (number-to-string (max 1 depth))
                       "-type" "d"
                       "-name" ".git"))))

  (defun my/git-discover-repositories ()
    "Discover repositories from configured roots."
    (let (repos)
      (dolist (entry (my/git-repository-roots))
        (let* ((root (expand-file-name (if (consp entry) (car entry) entry)))
               (depth (if (consp entry) (cdr entry) 2)))
          (when (file-directory-p root)
            (dolist (git-dir (or (my/git--scan-repos-fd root depth)
                                 (my/git--scan-repos-find root depth)))
              (push (file-name-as-directory
                     (file-name-directory (expand-file-name git-dir root)))
                    repos)))))
      (setq repos (seq-filter #'file-directory-p repos))
      (delete-dups (sort repos #'string-lessp))))

  (defun my/magit-status (&optional repo-dir)
    "Open Magit status for REPO-DIR or current repository."
    (interactive)
    (let* ((root (or repo-dir
                     (locate-dominating-file default-directory ".git")
                     (user-error "Not inside a Git repository"))))
      (magit-status (file-name-as-directory root))))

  (defun my/magit-open-repository ()
    "Pick a repository and open Magit."
    (interactive)
    (let* ((repos (my/git-discover-repositories))
           (choice (and repos
                        (completing-read
                         "Repository: " repos nil t nil
                         'my/magit-repository-history (car repos)))))
      (unless choice
        (user-error "No repositories found under `my/git-repository-directories'"))
      (my/magit-status choice)))

  (transient-define-prefix my/git-command-panel ()
    "Unified git panel backed by Magit."
    [["Magit"
      ("g" "Open current repository" my/magit-status)
      ("r" "Pick repository" my/magit-open-repository)
      ("f" "File dispatch" magit-file-dispatch)]])

  (use-package pinentry
    :init (setq epg-pinentry-mode 'loopback)
    :config (pinentry-start))

  (use-package blamer
    :defer t
    :custom (blamer-idle-time 0.3) (blamer-min-offset 70)
    :config (global-blamer-mode 1))

  (use-package diff-hl
    :init
    (add-hook 'prog-mode-hook #'diff-hl-mode)
    (add-hook 'text-mode-hook #'diff-hl-mode)
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

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

  (defun my/git-dashboard ()
    "Open Magit for current repository, or pick one."
    (interactive)
    (if (locate-dominating-file default-directory ".git")
        (call-interactively #'my/magit-status)
      (call-interactively #'my/magit-open-repository))))

;; ============================================================================
;; 8. LANGUAGES AND LSP
;; ============================================================================

(my/load-feature "lsp"
  (use-package lsp-mode
    :commands lsp-deferred
    :init
    (setq lsp-warn-no-matched-clients nil
          lsp-auto-guess-root t
          lsp-guess-root-without-session t
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
            (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3")
            (dart "https://github.com/ast-grep/tree-sitter-dart" "master")))
    ;; Mode remapping
    (let ((remaps '((python-mode . python-ts-mode)
                    (js-mode . js-ts-mode)
                    (typescript-mode . typescript-ts-mode)
                    (go-mode . go-ts-mode)
                    (rust-mode . rust-ts-mode)
                    (bash-mode . bash-ts-mode))))
      (when (fboundp 'dart-ts-mode)
        (push '(dart-mode . dart-ts-mode) remaps))
      (dolist (pair remaps)
        (add-to-list 'major-mode-remap-alist pair))))

  ;; LSP hooks for ts-modes
  (let ((ts-modes '(python-ts-mode js-ts-mode typescript-ts-mode tsx-ts-mode
                    go-ts-mode rust-ts-mode bash-ts-mode json-ts-mode)))
    (when (fboundp 'dart-ts-mode)
      (setq ts-modes (append ts-modes '(dart-ts-mode))))
    (dolist (mode ts-modes)
      (add-hook (intern (concat (symbol-name mode) "-hook")) #'lsp-deferred))))

(my/load-feature "dart"
  (use-package dart-mode
    :mode ("\\.dart\\'" . dart-mode))

  (use-package lsp-dart
    :after (lsp-mode dart-mode))

  (defun my/dart-enable-treesit-parser ()
    "Enable Dart tree-sitter parser in dart-mode when available."
    (when (and (fboundp 'treesit-parser-create)
               (fboundp 'treesit-language-available-p)
               (treesit-language-available-p 'dart))
      (ignore-errors
        (treesit-parser-create 'dart))))

  (add-hook 'dart-mode-hook #'my/dart-enable-treesit-parser)

  ;; Fallback path when dart-ts-mode is unavailable in this Emacs build.
  (add-hook 'dart-mode-hook #'lsp-deferred))

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
          markdown-fontify-code-blocks-natively t)
    :config
    ;; Markdown buffers use C-c p for fast preview.
    (define-key markdown-mode-map (kbd "C-c p") #'markdown-preview)))

;; ============================================================================
;; 11. LATEX WORKSPACE
;; ============================================================================

(my/load-feature "latex"
  (require 'doc-view)
  (setq doc-view-continuous t
        doc-view-resolution 180
        doc-view-use-scaling t)

  (defun my/doc-view-setup ()
    "Tune PDF preview behavior for reading workflow."
    ;; Keep PDF preview width at 100% of current preview window.
    (ignore-errors
      (doc-view-fit-width-to-window))
    ;; Mouse wheel: scroll current page, then flip page when reaching edge.
    (local-set-key [wheel-up] #'doc-view-scroll-down-or-previous-page)
    (local-set-key [wheel-down] #'doc-view-scroll-up-or-next-page)
    (local-set-key [mouse-4] #'doc-view-scroll-down-or-previous-page)
    (local-set-key [mouse-5] #'doc-view-scroll-up-or-next-page))
  (add-hook 'doc-view-mode-hook #'my/doc-view-setup)

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

  (defvar my/latex-last-project-directory nil
    "Last directory used by `my/latex-open-master-file'.")

  (defun my/latex--master-pdf-path (master-file)
    "Return expected PDF path for MASTER-FILE."
    (let* ((fallback (concat (file-name-sans-extension master-file) ".pdf"))
           (auctex-path
            (when (fboundp 'TeX-master-output-file)
              (ignore-errors
                (let ((out (TeX-master-output-file "pdf")))
                  (when (and (stringp out) (not (string-empty-p out)))
                    (if (file-name-absolute-p out)
                        out
                      (expand-file-name out (file-name-directory master-file)))))))))
      (if (and (stringp auctex-path)
               (not (string-empty-p auctex-path))
               (not (string-match-p "<none>" (downcase auctex-path))))
          auctex-path
        fallback)))

  (defun my/latex--pdf-preview-buffer (pdf-path)
    "Return preview buffer for PDF-PATH, fallback to hint buffer when missing."
    (if (file-exists-p pdf-path)
        (find-file-noselect pdf-path)
      (let ((buf (get-buffer-create "*LaTeX PDF Preview*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "PDF preview is not available yet.\n\n")
            (insert (format "Expected file:\n%s\n\n" pdf-path))
            (insert "Run AUCTeX compile in the TeX buffer:\n")
            (insert "  M-x TeX-command-master\n")
            (insert "Then press `g` here or run `C-c l` again.\n")
            (special-mode)))
        buf)))

  (defun my/latex-open-master-file ()
    "Pick TeX directory and master file, then open TeX/PDF two-column layout."
    (interactive)
    (unless (or (featurep 'tex)
                (require 'tex-site nil t)
                (require 'tex nil t))
      (user-error "AUCTeX is not available"))
    (let* ((seed-dir (cond
                      ((and (stringp my/latex-last-project-directory)
                            (file-directory-p my/latex-last-project-directory))
                       my/latex-last-project-directory)
                      ((buffer-file-name) (file-name-directory (buffer-file-name)))
                      (t default-directory)))
           (project-dir
            (file-name-as-directory
             (expand-file-name
              (read-directory-name "TeX directory: " seed-dir nil t))))
           (master-file
            (expand-file-name
             (read-file-name
              "TeX master file: " project-dir nil t nil
              (lambda (file)
                (or (file-directory-p file)
                    (string-match-p "\\.tex\\'" file)))))))
      (unless (and (file-exists-p master-file)
                   (not (file-directory-p master-file))
                   (string-match-p "\\.tex\\'" master-file))
        (user-error "Please select a valid .tex file"))
      (setq my/latex-last-project-directory project-dir)
      (let ((tex-buffer (find-file-noselect master-file)))
        (when (display-graphic-p)
          (set-frame-parameter nil 'fullscreen 'maximized))
        (delete-other-windows)
        (switch-to-buffer tex-buffer)
        ;; Main file itself should be master in AUCTeX.
        (when (derived-mode-p 'tex-mode)
          (setq-local TeX-master t))
        (let* ((pdf-path
                (with-current-buffer tex-buffer
                  (my/latex--master-pdf-path master-file)))
               (pdf-buffer (my/latex--pdf-preview-buffer pdf-path))
               (right-win (split-window-right)))
          (set-window-buffer right-win pdf-buffer)
          (with-selected-window right-win
            (when (derived-mode-p 'doc-view-mode)
              (ignore-errors
                (doc-view-fit-width-to-window)))))
        (balance-windows))))

  ;; `C-c l': pick TeX directory first, then choose master file.
  (global-set-key (kbd "C-c l") #'my/latex-open-master-file)
  )

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
           ("C-c >" . mc/mark-all-like-this))
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
  (global-set-key (kbd "s-G") #'my/git-dashboard)
  (global-set-key (kbd "C-c g") #'my/git-dashboard)
  (global-set-key (kbd "s-d") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-x d") #'dirvish-dwim)

  ;; Terminal super key bindings via CSI u sequences over SSH
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
  (global-set-key (kbd "C-c G") #'my/git-command-panel)
  (global-set-key (kbd "C-c /") #'consult-ripgrep)
  (global-set-key (kbd "C-c ?") #'my/ast-grep-search)
  (global-set-key (kbd "C-c b") #'consult-buffer)
  (global-set-key (kbd "C-c o") #'find-file)
  (global-set-key (kbd "C-c u") #'vundo)
  (global-set-key (kbd "C-c [") #'tab-previous)
  (global-set-key (kbd "C-c ]") #'tab-next)
  (global-set-key (kbd "C-c n") #'tab-new)
  (global-set-key (kbd "C-c x") #'tab-close)
  (global-set-key (kbd "C-c d") #'my/goto-definition-smart)
  (global-set-key (kbd "C-c j") #'consult-imenu)
  (global-set-key (kbd "C-x g") #'my/git-dashboard)

  ;; Diagnostics
  (defun my/open-diagnostics ()
    (interactive)
    (cond
     ((fboundp 'consult-flymake) (consult-flymake))
     ((fboundp 'flymake-show-buffer-diagnostics) (flymake-show-buffer-diagnostics))
     (t (user-error "No diagnostics UI available"))))
  (global-set-key (kbd "C-c !") #'my/open-diagnostics)

  ;; Eat keybindings
  (with-eval-after-load 'eat
    (let ((map eat-mode-map))
      (define-key map (kbd "C-y") #'my/eat-clipboard-yank)))

  ;; LSP keybindings
  (with-eval-after-load 'lsp-mode
    (define-key lsp-mode-map (kbd "C-c r") #'my/find-references-smart)
    (define-key lsp-mode-map (kbd "C-c .") #'lsp-execute-code-action)
    (define-key lsp-mode-map (kbd "C-c f") #'lsp-format-buffer)
    (define-key lsp-mode-map (kbd "C-c i") #'lsp-organize-imports))

  ;; Python keybindings
  (with-eval-after-load 'python
    (define-key python-mode-map (kbd "C-c t") #'python-pytest))

  ;; Markdown keybindings
  )

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
