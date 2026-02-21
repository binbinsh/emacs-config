;;; init.el --- Main initialization -*- lexical-binding: t -*-
;;
;; Minimal synchronous setup. All features load async via post-init.el.
;;
;; Structure:
;; 1. Package Bootstrap
;; 2. User Profile
;; 3. Cache Paths
;; 4. Essential UI (synchronous)
;; 5. Theme and Fonts
;; 6. Async Loading Trigger

;; ============================================================================
;; 1. PACKAGE BOOTSTRAP
;; ============================================================================

(require 'package)
(unless package--initialized (package-initialize))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Better subprocess I/O throughput (benefits LSP)
(setq read-process-output-max (* 3 1024 1024))

;; ~/.local/bin for uv and other tools
(let ((local-bin (expand-file-name "~/.local/bin")))
  (when (file-directory-p local-bin)
    (add-to-list 'exec-path local-bin)
    (setenv "PATH" (concat local-bin path-separator (getenv "PATH")))))

;; ============================================================================
;; 2. USER PROFILE
;; ============================================================================

(setq user-full-name "Binbin Shen")
(setq user-mail-address "bbs2021@sjtu.edu.cn")

;; Magit repository roots
(setq magit-repository-directories
      (list (cons (expand-file-name "~/Projects/") 2)))

;; Shell for terminal
(defvar my/terminal-shell
  (or (getenv "SHELL")
      (if (eq system-type 'darwin) "/bin/zsh" "/bin/bash")))

;; M-x package-install RET clipetty RET
(use-package clipetty
  :ensure t
  :hook (tty-setup . my/tty-clipboard-setup))

;; ============================================================================
;; 3. CACHE PATHS (my-emacs-cache-directory from early-init.el)
;; ============================================================================

(setq vc-make-backup-files t)
(setq temporary-file-directory (expand-file-name "tmp" my-emacs-cache-directory))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" my-emacs-cache-directory) t)))
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" my-emacs-cache-directory))
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups" my-emacs-cache-directory))))
(when (boundp 'lock-file-name-transforms)
  (setq lock-file-name-transforms
        `((".*" ,(expand-file-name "locks/" my-emacs-cache-directory) t))))
(setq nsm-settings-file (expand-file-name "network-security.data" my-emacs-cache-directory))
(setq eshell-directory-name (expand-file-name "eshell" my-emacs-cache-directory))
(setq tramp-auto-save-directory (expand-file-name "tramp" my-emacs-cache-directory))
(setq tramp-persistency-file-name (expand-file-name "tramp/tramp" my-emacs-cache-directory))
(setq recentf-save-file (expand-file-name "recentf" my-emacs-cache-directory))
(setq savehist-file (expand-file-name "savehist" my-emacs-cache-directory))
(setq save-place-file (expand-file-name "places" my-emacs-cache-directory))
(setq bookmark-default-file (expand-file-name "bookmarks/bookmarks" my-emacs-cache-directory))
(setq url-configuration-directory (expand-file-name "url/" my-emacs-cache-directory))
(setq url-cache-directory (expand-file-name "url/cache" my-emacs-cache-directory))
(setq url-history-file (expand-file-name "url/history" my-emacs-cache-directory))
(setq project-list-file (expand-file-name "projects" my-emacs-cache-directory))
(setq transient-history-file (expand-file-name "transient/history.el" my-emacs-cache-directory))
(setq transient-levels-file (expand-file-name "transient/levels.el" my-emacs-cache-directory))
(setq transient-values-file (expand-file-name "transient/values.el" my-emacs-cache-directory))
(setq dirvish-cache-dir (expand-file-name "dirvish" my-emacs-cache-directory))
(setq lsp-session-file (expand-file-name ".lsp-session-v1" my-emacs-cache-directory))
(setq dap-breakpoints-file (expand-file-name ".dap-breakpoints" my-emacs-cache-directory))
(setq lsp-server-install-dir (expand-file-name "lsp/" my-emacs-cache-directory))
(setq svg-lib-icons-dir (expand-file-name "svg-lib/" my-emacs-cache-directory))

;; Disable custom file (write to cache, never load)
(setq custom-file (expand-file-name "custom.el" my-emacs-cache-directory))

;; TRAMP async previews for SSH connections (Dirvish/Dired remote previews).
(with-eval-after-load 'tramp
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process))

;; ============================================================================
;; 4. ESSENTIAL UI (SYNCHRONOUS)
;; ============================================================================

(setq inhibit-startup-screen nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq frame-resize-pixelwise t)
(setq column-number-mode t)

(defconst my/startup-profile-buffer-name "*GNU Emacs*"
  "Name of the default Emacs startup buffer.")

(defvar my/startup-profile-modules (make-hash-table :test 'equal)
  "Hashtable mapping module names to startup profiling plists.")

(defvar my/startup-profile-module-order nil
  "Module names in registration order.")

(defvar my/startup-profile--region nil
  "Cons cell of markers delimiting the startup profile section.")

(defvar my/startup-profile--refresh-timer nil
  "Timer used to coalesce startup profile redraws.")

(defun my/display-startup-screen (&optional concise)
  "Display a minimal startup screen with profiling only."
  (let ((splash-buffer (get-buffer-create my/startup-profile-buffer-name)))
    (with-current-buffer splash-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory
              (if (boundp 'command-line-default-directory)
                  command-line-default-directory
                default-directory))
        (when (boundp 'splash-screen-keymap)
          (use-local-map splash-screen-keymap))
        (setq tab-width 8
              buffer-read-only t)
        (set-buffer-modified-p nil)))
    (if concise
        (display-buffer splash-buffer)
      (switch-to-buffer splash-buffer)))
  (my/startup-profile-refresh))

(advice-add 'display-startup-screen :override #'my/display-startup-screen)

(defun my/startup-profile-register-module (name scheduled-delay)
  "Register startup module NAME with SCHEDULED-DELAY in seconds."
  (unless (member name my/startup-profile-module-order)
    (setq my/startup-profile-module-order
          (append my/startup-profile-module-order (list name))))
  (let ((entry (or (gethash name my/startup-profile-modules) (list :name name))))
    (setq entry (plist-put entry :status 'pending))
    (setq entry (plist-put entry :scheduled-delay scheduled-delay))
    (puthash name entry my/startup-profile-modules))
  (my/startup-profile-queue-refresh))

(defun my/startup-profile-record-module (name begin end &optional status)
  "Record startup runtime for module NAME between BEGIN and END."
  (let* ((entry (or (gethash name my/startup-profile-modules) (list :name name)))
         (elapsed (float-time (time-subtract end begin)))
         (since-start (float-time (time-subtract end before-init-time))))
    (setq entry (plist-put entry :status (or status 'ok)))
    (setq entry (plist-put entry :elapsed elapsed))
    (setq entry (plist-put entry :since-start since-start))
    (puthash name entry my/startup-profile-modules))
  (my/startup-profile-queue-refresh))

(defun my/startup-profile--entries ()
  "Return startup profiling entries sorted by module runtime."
  (let (entries)
    (dolist (name my/startup-profile-module-order)
      (let ((entry (copy-sequence
                    (or (gethash name my/startup-profile-modules)
                        (list :name name)))))
        (setq entry (plist-put entry :name name))
        (push entry entries)))
    (sort (nreverse entries)
          (lambda (a b)
            (let ((a-elapsed (plist-get a :elapsed))
                  (b-elapsed (plist-get b :elapsed)))
              (cond
               ((and a-elapsed b-elapsed) (> a-elapsed b-elapsed))
               (a-elapsed t)
               (b-elapsed nil)
               (t (< (or (plist-get a :scheduled-delay) 0.0)
                     (or (plist-get b :scheduled-delay) 0.0)))))))))

(defun my/startup-profile--bar (value max-value width)
  "Return a WIDTH-char ASCII bar for VALUE relative to MAX-VALUE."
  (let* ((safe-max (max max-value 0.0001))
         (filled (min width (max 0 (round (* width (/ value safe-max))))))
         (empty (max 0 (- width filled))))
    (concat (make-string filled ?#) (make-string empty ?-))))

(defun my/startup-profile--render ()
  "Render startup profile text."
  (let* ((entries (my/startup-profile--entries))
         (init-end (or after-init-time (current-time)))
         (total-init (float-time (time-subtract init-end before-init-time)))
         (loaded-count 0)
         (max-elapsed 0.0)
         (table-lines nil))
    (dolist (entry entries)
      (let ((elapsed (plist-get entry :elapsed)))
        (when elapsed
          (setq loaded-count (1+ loaded-count))
          (setq max-elapsed (max max-elapsed elapsed)))))
    (dolist (entry entries)
      (let* ((name (or (plist-get entry :name) "unknown"))
             (elapsed (plist-get entry :elapsed))
             (since-start (plist-get entry :since-start))
             (status (or (plist-get entry :status) 'pending))
             (bar (my/startup-profile--bar (or elapsed 0.0) max-elapsed 18)))
        (push (format "%-20s %9s %9s %8s |%s|"
                      (truncate-string-to-width name 20 nil nil t)
                      (if elapsed (format "%.1f" (* elapsed 1000.0)) "--")
                      (if since-start (format "%.1f" (* since-start 1000.0)) "--")
                      (upcase (symbol-name status))
                      bar)
              table-lines)))
    (setq table-lines (nreverse table-lines))
    (concat
     "Startup Performance\n"
     "-------------------\n"
     (format "Total init: %.3fs | GC cycles: %d | Modules loaded: %d/%d\n"
             total-init gcs-done loaded-count (length entries))
     "self(ms)=module runtime, at+(ms)=time since Emacs process start\n"
     "relative=bar scaled to the slowest module (# means heavier)\n\n"
     (format "%-20s %9s %9s %8s %s\n" "Module" "self(ms)" "at+(ms)" "status" "relative")
     (make-string 76 ?-)
     "\n"
     (if table-lines
         (mapconcat #'identity table-lines "\n")
       "No post-init modules registered yet."))))

(defun my/startup-profile-refresh ()
  "Refresh startup profile in the default startup buffer."
  (setq my/startup-profile--refresh-timer nil)
  (let ((buffer (get-buffer my/startup-profile-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (section (concat "\n" (my/startup-profile--render) "\n")))
          (if (and my/startup-profile--region
                   (markerp (car my/startup-profile--region))
                   (markerp (cdr my/startup-profile--region))
                   (eq (marker-buffer (car my/startup-profile--region)) (current-buffer))
                   (eq (marker-buffer (cdr my/startup-profile--region)) (current-buffer)))
              (let ((start (car my/startup-profile--region))
                    (end (cdr my/startup-profile--region)))
                (delete-region start end)
                (goto-char start)
                (let ((new-start (point)))
                  (insert section)
                  (set-marker (car my/startup-profile--region) new-start)
                  (set-marker (cdr my/startup-profile--region) (point))))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (let ((start (point)))
              (insert section)
              (setq my/startup-profile--region
                    (cons (copy-marker start t)
                          (copy-marker (point) nil)))))
          (set-buffer-modified-p nil))))))

(defun my/startup-profile-queue-refresh ()
  "Queue startup profile redraw."
  (when (timerp my/startup-profile--refresh-timer)
    (cancel-timer my/startup-profile--refresh-timer))
  (setq my/startup-profile--refresh-timer
        (run-with-idle-timer 0.03 nil #'my/startup-profile-refresh)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer 0 nil #'my/startup-profile-refresh)))

;; Smooth scrolling
(pixel-scroll-precision-mode 1)
(setq scroll-margin 8 scroll-conservatively 101 auto-window-vscroll nil)

;; Visual feedback
(global-hl-line-mode 1)
(show-paren-mode 1)

;; History and state
(recentf-mode 1)
(save-place-mode 1)
(savehist-mode 1)

;; Line numbers
(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))
(add-hook 'text-mode-hook (lambda () (display-line-numbers-mode 1)))

;; macOS PATH sync
(use-package exec-path-from-shell
  :if (and (memq system-type '(darwin)) (display-graphic-p))
  :init (setq exec-path-from-shell-arguments '("-l"))
  :config (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "LANG" "LC_ALL" "SSH_AUTH_SOCK")))

;; macOS modifier keys (Option as Meta; keep right Option for symbols)
(when (eq system-type 'darwin)
  (when (boundp 'mac-option-modifier)
    (setq mac-option-modifier 'meta
          mac-right-option-modifier 'none))
  (when (boundp 'ns-option-modifier)
    (setq ns-option-modifier 'meta
          ns-right-option-modifier 'none)))

;; Clipboard integration (macOS + Wayland, GUI + TTY)
(defun my/clipboard-backend ()
  "Return the available clipboard backend symbol, or nil."
  (cond
   ((eq system-type 'darwin)
    (when (and (executable-find "pbcopy") (executable-find "pbpaste"))
      'pbcopy))
   ((and (or (getenv "WAYLAND_DISPLAY") (getenv "WAYLAND_SOCKET"))
         (executable-find "wl-copy")
         (executable-find "wl-paste"))
    'wl-clipboard)
   (t nil)))

(defun my/clipboard-set (text)
  "Copy TEXT to the system clipboard when available."
  (let ((process-connection-type nil)
        (backend (my/clipboard-backend)))
    (pcase backend
      ('pbcopy
       (with-temp-buffer
         (insert text)
         (call-process-region (point-min) (point-max) "pbcopy" nil 0)))
      ('wl-clipboard
       (with-temp-buffer
         (insert text)
         (call-process-region (point-min) (point-max) "wl-copy" nil 0)))))
  text)

(defun my/clipboard-get ()
  "Return clipboard contents, or nil."
  (let ((process-connection-type nil)
        (backend (my/clipboard-backend))
        (text nil))
    (pcase backend
      ('pbcopy
       (with-temp-buffer
         (call-process "pbpaste" nil t nil)
         (setq text (buffer-string))))
      ('wl-clipboard
       (with-temp-buffer
         (call-process "wl-paste" nil t nil "-n")
         (setq text (buffer-string)))))
    (and text (> (length text) 0) text)))

(defun my/clipboard-ensure-functions ()
  "Ensure kill ring and system clipboard interoperate."
  (setq select-enable-clipboard t
        select-enable-primary nil
        save-interprogram-paste-before-kill t)
  (let ((backend (my/clipboard-backend)))
    (cond
     (backend
      (setq interprogram-cut-function #'my/clipboard-set
            interprogram-paste-function #'my/clipboard-get))
     ((null interprogram-cut-function)
      (setq interprogram-cut-function #'ignore)))))

(defun my/tty-clipboard-setup ()
  "Setup clipboard helpers for TTY frames."
  (my/clipboard-ensure-functions)
  (global-clipetty-mode 1)
  (unless (display-graphic-p)
    (require 'xt-mouse)
    (xterm-mouse-mode 1)
    (setq mouse-drag-copy-region 'non-empty)))

(add-hook 'after-init-hook #'my/clipboard-ensure-functions)

(when (eq system-type 'darwin)
  (setq select-enable-clipboard t)
  (global-set-key (kbd "s-c") #'kill-ring-save)
  (global-set-key (kbd "s-x") #'kill-region)
  (global-set-key (kbd "s-v") #'yank))

;; GC smoothing
(use-package gcmh
  :init
  (setq gcmh-idle-delay 1.0 gcmh-high-cons-threshold (* 16 1024 1024))
  (add-hook 'emacs-startup-hook #'gcmh-mode t))

;; ============================================================================
;; 5. THEME AND FONTS
;; ============================================================================

;; Load theme synchronously (user sees this immediately)
(use-package monokai-pro-theme
  :config
  (load-theme 'monokai-pro-octagon t))

;; Font setup
(require 'cl-lib)

(defun my/find-first-font (fonts)
  "Return first available font from FONTS list."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(defvar my/bundled-jetbrains-mono-nerd-font-ensured nil
  "Non-nil once bundled JetBrains Mono Nerd Font installation has been attempted this session.")

(defun my/ensure-bundled-jetbrains-mono-nerd-font ()
  "Install bundled JetBrains Mono Nerd font for current user, preferring repo copy."
  (unless my/bundled-jetbrains-mono-nerd-font-ensured
    (setq my/bundled-jetbrains-mono-nerd-font-ensured t)
    (let* ((source (expand-file-name "assets/JetBrainsMonoNerdFontMono-Regular.ttf" user-emacs-directory))
           (dest-dir (cond
                      ((eq system-type 'darwin) (expand-file-name "~/Library/Fonts"))
                      ((eq system-type 'gnu/linux) (expand-file-name "~/.local/share/fonts"))
                      (t nil)))
           (dest (and dest-dir (expand-file-name "JetBrainsMonoNerdFontMono-Regular.ttf" dest-dir))))
      (when (and dest (file-exists-p source))
        (make-directory dest-dir t)
        (copy-file source dest t)
        (when (and (eq system-type 'gnu/linux) (executable-find "fc-cache"))
          (call-process "fc-cache" nil nil nil "-f" dest-dir))
        (when (fboundp 'clear-font-cache)
          (clear-font-cache))))))

(defun my/apply-fonts (&optional frame)
  "Apply preferred fonts to FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (my/ensure-bundled-jetbrains-mono-nerd-font)
      (let* ((system-default-english-candidates
              (cond
               ((eq system-type 'darwin) '("JetBrainsMono NFM"))
               ((eq system-type 'gnu/linux) '("JetBrainsMono NFM"))
               (t '("Monospace"))))
             (english (or (my/find-first-font system-default-english-candidates) "Monospace"))
             (cjk (or (my/find-first-font '("PingFang SC" "Noto Sans CJK SC")) "Noto Sans CJK SC"))
             (emoji (my/find-first-font '("Apple Color Emoji" "Noto Color Emoji"))))
        (set-face-attribute 'default nil :font (font-spec :family english :size 15))
        (dolist (charset '(han kana cjk-misc bopomofo))
          (set-fontset-font t charset (font-spec :name cjk)))
        (when emoji (set-fontset-font t 'emoji (font-spec :name emoji) nil 'prepend))
        ;; Powerline symbols (U+E0A0-U+E0D4) - use Nerd Font
        (set-fontset-font t '(#xe0a0 . #xe0d4) (font-spec :family english))
        ;; Private Use Area for nerd-icons
        (set-fontset-font t '(#xf000 . #xf8ff) (font-spec :family english))
        (setq nerd-icons-font-family "Symbols Nerd Font Mono")))))

(add-hook 'emacs-startup-hook #'my/apply-fonts)
(add-hook 'after-make-frame-functions #'my/apply-fonts)

;; ============================================================================
;; 6. DIRED ENHANCEMENT (immediate for emacs -nw .)
;; ============================================================================

;; Dired sorting: directories first
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)
(setq ls-lisp-dirs-first t)

;; Dirvish: modern dired UI - loaded early for `emacs -nw .` support
(use-package dirvish
  :demand t
  :custom
  ;; nerd-icons 图标
  (dirvish-attributes '(nerd-icons vc-state subtree-state collapse file-size file-time))
  (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format '(:left (sort omit symlink) :right (index)))
  (dirvish-use-header-line t)
  (dirvish-use-mode-line nil)
  (dirvish-hide-details t)
  (dirvish-hide-cursor t)
  (dirvish-window-fringe 8)
  ;; 预览功能
  (dirvish-preview-dispatchers '(image gif video audio epub archive pdf))
  (dirvish-default-layout '(0 0.4 0.6))  ;; 左40%文件列表，右60%预览
  :config
  (dirvish-override-dired-mode)
  (with-eval-after-load 'dirvish-subtree
    (setq dirvish-subtree-state-style 'nerd
          dirvish-subtree-always-show-state t))
  (with-eval-after-load 'dirvish-side
    (setq dirvish-side-width 32
          dirvish-side-attributes '(nerd-icons subtree-state)
          dirvish-side-header-line-format '(:left (project) :right (free-space))))
  :bind (:map dirvish-mode-map
         ("/" . dirvish-narrow)))

;; ============================================================================
;; 7. VTERM TOGGLE (immediate availability)
;; ============================================================================

(defun my/toggle-vterm ()
  "Toggle a bottom vterm panel (30% height)."
  (interactive)
  (let* ((name "*vterm*")
         (buf (get-buffer name))
         (win (and buf (get-buffer-window buf))))
    (if (and buf (window-live-p win))
        (delete-window win)
      (let* ((target-lines (floor (* 0.3 (frame-height))))
             (new-win (split-window (selected-window) (- target-lines) 'below)))
        (select-window new-win)
        (if (buffer-live-p buf)
            (switch-to-buffer buf)
          (vterm))))))

(global-set-key (kbd "C-c v") #'my/toggle-vterm)
(global-set-key (kbd "s-`") #'my/toggle-vterm)

;; ============================================================================
;; 8. ASYNC LOADING TRIGGER
;; ============================================================================

(defun my/load-post-init ()
  "Load post-init.el asynchronously."
  (let ((post-init (expand-file-name "post-init.el" user-emacs-directory)))
    (when (file-exists-p post-init)
      (load post-init nil t))))

;; Load after 0.1s idle - gives Emacs time to render frame first
(run-with-idle-timer 0.1 nil #'my/load-post-init)

;; Native compile in background
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p)
           (fboundp 'native-compile-async))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (ignore-errors
                (native-compile-async user-emacs-directory 'recursively)))))

;;; init.el ends here
