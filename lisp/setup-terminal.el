(add-hook 'vterm-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode -1)))

;; Core customizations and helpers for a Termius-like terminal workflow.

;; Dependencies used by this module
(require 'seq)
(require 'subr-x) ;; string-join, string-trim, string-remove-suffix
(require 'cl-lib)
;; Load AI helpers only when needed to avoid startup overhead
(autoload 'my/ai-suggest-shell "setup-ai" nil t)
;; Reuse existing vterm selection helper from snippets module
(autoload 'my/snippet--ensure-local-vterm "setup-snippets" nil t)

(defgroup my-terminal nil
  "Termius-like terminal workflow and AI-assisted command suggestions."
  :group 'tools
  :prefix "my/terminal-")

;; ----------------------------------------------------------------------------
;; TRAMP performance tuning
;; ----------------------------------------------------------------------------
;; - Enable SSH multiplexing (ControlMaster/ControlPersist)
;; - Cache remote completions to reduce repeated stat calls
;; - Disable VC on remote paths to avoid expensive VCS probes over TRAMP
(setq tramp-default-method "ssh")
(with-eval-after-load 'tramp
  (setq tramp-use-ssh-controlmaster-options t
        tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath=~/.ssh/tramp.%%C -o ControlPersist=yes"
        remote-file-name-inhibit-cache nil
        tramp-completion-reread-directory-timeout 600
        tramp-verbose 1
        tramp-chunksize 65536)
  (when (boundp 'vc-ignore-dir-regexp)
    (setq vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))))

(defcustom my/terminal-backend 'vterm
  "Terminal backend to use. Currently only `vterm' is supported."
  :type '(choice (const :tag "vterm" vterm))
  :group 'my-terminal)

(defcustom my/terminal-shell (or (getenv "SHELL")
                                 (if (eq system-type 'darwin) "/bin/zsh" "/bin/bash"))
  "Shell executable used by the terminal backend."
  :type 'string
  :group 'my-terminal)

(defconst my/vterm-panel-buffer-name "vterm"
  "Reserved name for the global vterm panel toggled via `C-c v'.")

(defun my/vterm--rename-eligible-p (&optional buffer)
  "Return non-nil when BUFFER should be auto-renamed by our vterm helpers."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'vterm-mode)
         (not (string= (buffer-name) my/vterm-panel-buffer-name)))))

(defun my/vterm--current-directory (&optional buffer)
  "Return the best-effort directory string for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((and (fboundp 'vterm--get-pwd) (bound-and-true-p vterm--term))
      (or (ignore-errors (vterm--get-pwd)) default-directory))
     (t default-directory))))

(defun my/vterm--abbreviate-path (path remote)
  "Return a readable variant of PATH. REMOTE indicates remote paths."
  (let ((clean (directory-file-name (or path ""))))
    (cond
     ((string-empty-p clean) "/")
     ((and remote clean) clean)
     ((fboundp 'abbreviate-file-name)
      (abbreviate-file-name clean))
     (t clean))))

(defun my/vterm--format-buffer-name (&optional buffer)
  "Build the desired buffer name for BUFFER based on directory/host."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((dir (my/vterm--current-directory))
           (remote-host (and dir (file-remote-p dir 'host)))
           (remote-port (and dir (file-remote-p dir 'port)))
           (remote-path (and dir (file-remote-p dir 'localname)))
           (path (my/vterm--abbreviate-path (or remote-path dir) remote-host))
           (host (or (and remote-host
                          (if remote-port
                              (format "%s#%s" remote-host remote-port)
                            remote-host))
                     (system-name)
                     "localhost")))
      (when (and path host)
        (format "vterm: %s@%s" path host)))))

(defun my/vterm--refresh-buffer-name (&rest _)
  "Rename the current vterm buffer unless it is the fixed global panel."
  (when (my/vterm--rename-eligible-p)
    (when-let ((new-name (my/vterm--format-buffer-name)))
      (unless (string= (buffer-name) new-name)
        (rename-buffer new-name t)))))

(use-package vterm
  :commands (vterm)
  :init
  ;; Set desired shell before vterm loads; harmless if vterm not yet loaded
  (setq vterm-shell my/terminal-shell))

(with-eval-after-load 'vterm
  ;; Speed up vterm timer
  (setq vterm-timer-delay nil)
  ;; Ensure paste keys are handled by Emacs in vterm
  (when (boundp 'vterm-keymap-exceptions)
    (dolist (k '("s-v"))
      (add-to-list 'vterm-keymap-exceptions k)))
  (add-hook 'vterm-mode-hook #'my/vterm--refresh-buffer-name)
  (advice-add 'vterm--set-directory :after #'my/vterm--refresh-buffer-name))

(defun my/terminal--ensure-vterm ()
  "Ensure vterm is available. Return non-nil if ok."
  (and (eq my/terminal-backend 'vterm)
       (or (featurep 'vterm)
           (require 'vterm nil t))))

(defun my/terminal-open ()
  "Open a new terminal session using the configured backend."
  (interactive)
  (cond
   ((and (eq my/terminal-backend 'vterm) (my/terminal--ensure-vterm))
    (let ((vterm-shell my/terminal-shell))
      (vterm (format "vterm-%s" (format-time-string "%Y%m%d-%H%M%S")))))
   (t (message "No supported terminal backend available."))))

(defun my/terminal-open-here (&optional directory)
  "Open a terminal in DIRECTORY or the current `default-directory'."
  (interactive)
  (let ((dir (or directory default-directory)))
    (cond
     ((and (eq my/terminal-backend 'vterm) (my/terminal--ensure-vterm))
      (let ((default-directory dir)
            (vterm-shell my/terminal-shell))
        (vterm (format "vterm-%s" (format-time-string "%Y%m%d-%H%M%S")))))
     (t (message "No supported terminal backend available.")))))

(defun my/terminal-open-named (name)
  "Open a terminal buffer with NAME using the configured backend."
  (interactive "sTerminal name: ")
  (cond
   ((and (eq my/terminal-backend 'vterm) (my/terminal--ensure-vterm))
    (let ((vterm-shell my/terminal-shell))
      (vterm name)))
   (t (message "No supported terminal backend available."))))

;; Find or create a vterm in the current frame, preferring the current buffer
;; if it's already a vterm. Otherwise reuse a visible vterm window in this
;; frame, or create a uniquely named one with a timestamp.

;; ----------------------------------------------------------------------------
;; Host profiles and SSH / TRAMP helpers
;; ----------------------------------------------------------------------------

(defcustom my/terminal-hosts
  '()
  "List of host profiles as plists.
Each profile supports keys:
  :name (string) unique name
  :host (string) hostname or IP
  :user (string) ssh user
  :port (number) ssh port (optional)
  :identity-file (string) path to private key (optional)
  :proxy-jump (string) proxy host user@host (optional)
  :tunnels (list) list of plists with keys
           :type (`L' or `R'), :local (`[host:]port'), :remote (`[host:]port')
  :labels (list of strings) arbitrary tags.
The structure is intentionally flexible and editable via customize."
  :type 'sexp
  :group 'my-terminal)

(defun my/terminal--host-names ()
  (mapcar (lambda (h) (plist-get h :name)) my/terminal-hosts))

(defun my/terminal--find-host (name)
  (seq-find (lambda (h) (string-equal (plist-get h :name) name)) my/terminal-hosts))

(defun my/terminal--build-ssh-command (host)
  "Return an ssh command string for HOST plist."
  (let* ((user (plist-get host :user))
         (hostname (plist-get host :host))
         (port (plist-get host :port))
         (identity (plist-get host :identity-file))
         (proxy (plist-get host :proxy-jump))
         (parts (delq nil
                      (list "ssh"
                            (when port (format "-p %s" port))
                            (when identity (format "-i %s" identity))
                            (when proxy (format "-J %s" proxy))
                            (if user
                                (format "%s@%s" user hostname)
                              (or hostname ""))))))
    (string-join parts " ")))

(defun my/terminal-ssh-connect ()
  "Pick a host profile and connect via ssh in a vterm local to this frame."
  (interactive)
  (if (null my/terminal-hosts)
      (message "No host profiles. Use M-x customize-variable my/terminal-hosts to add.")
    (let* ((name (completing-read "Host: " (my/terminal--host-names) nil t))
           (host (my/terminal--find-host name))
           (cmd (my/terminal--build-ssh-command host)))
      (if (not (my/terminal--ensure-vterm))
          (message "vterm not available.")
        (let ((buf (my/snippet--ensure-local-vterm)))
          (when (buffer-live-p buf)
            (pop-to-buffer buf)
            (with-current-buffer buf
              (when (and (boundp 'vterm--process) vterm--process)
                (vterm-send-string cmd)
                (vterm-send-return)))))))))

(defun my/terminal--tramp-path-for-host (host &optional path)
  "Build a TRAMP path for HOST plist. Optional PATH defaults to ~/"
  (let* ((user (plist-get host :user))
         (hostname (plist-get host :host))
         (port (plist-get host :port))
         (proxy (plist-get host :proxy-jump))
         (base (if user
                   (format "%s@%s" user hostname)
                 (or hostname "")))
         (hop (when proxy (format "ssh:%s|" proxy)))
         (port-frag (when port (format "#%s" port)))
         (dir (or path "~/")))
    (format "/%sssh:%s%s:%s" (or hop "") base (or port-frag "") dir)))

(defun my/terminal-remote-dired (&optional path)
  "Open a Dired buffer for a host profile via TRAMP."
  (interactive)
  (require 'tramp)
  (require 'tramp-sh)
  (if (null my/terminal-hosts)
      (message "No host profiles. Use M-x customize-variable my/terminal-hosts to add.")
    (let* ((name (completing-read "Host (Dired): " (my/terminal--host-names) nil t))
           (host (my/terminal--find-host name))
           (tramp-path (my/terminal--tramp-path-for-host host path)))
      (dired tramp-path))))

;; ----------------------------------------------------------------------------
;; SSH tunnels management
;; ----------------------------------------------------------------------------

(defvar my/terminal--tunnel-process-table (make-hash-table :test 'equal)
  "Mapping from tunnel key to running ssh tunnel process.")

(defun my/terminal--tunnel-key (host tunnel)
  (format "%s|%s|%s"
          (plist-get host :name)
          (plist-get tunnel :type)
          (or (plist-get tunnel :local) (plist-get tunnel :remote) "")))

(defun my/terminal--build-tunnel-command (host tunnel)
  (let* ((type (plist-get tunnel :type))
         (local (plist-get tunnel :local))
         (remote (plist-get tunnel :remote))
         (flag (if (string-equal type "R") "-R" "-L"))
         (user (plist-get host :user))
         (hostname (plist-get host :host))
         (port (plist-get host :port))
         (identity (plist-get host :identity-file))
         (proxy (plist-get host :proxy-jump))
         (target (if user (format "%s@%s" user hostname) hostname))
         (parts (delq nil
                      (list "ssh" "-N"
                            (format "%s %s:%s" flag (or local "") (or remote ""))
                            "-o" "ExitOnForwardFailure=yes"
                            "-o" "ServerAliveInterval=60"
                            (when port (format "-p %s" port))
                            (when identity (format "-i %s" identity))
                            (when proxy (format "-J %s" proxy))
                            target))))
    (mapconcat #'identity parts " ")))

(defun my/terminal-tunnel-start ()
  "Start a configured SSH tunnel for a chosen host."
  (interactive)
  (if (null my/terminal-hosts)
      (message "No host profiles. Use M-x customize-variable my/terminal-hosts to add.")
    (let* ((name (completing-read "Host (tunnel): " (my/terminal--host-names) nil t))
           (host (my/terminal--find-host name))
           (tunnels (plist-get host :tunnels)))
      (if (null tunnels)
          (message "No tunnels configured for %s" name)
        (let* ((labels (mapcar (lambda (t)
                                 (format "%s %s->%s"
                                         (plist-get t :type)
                                         (or (plist-get t :local) "?")
                                         (or (plist-get t :remote) "?")))
                               tunnels))
               (choice (completing-read "Tunnel: " labels nil t))
               (sel (nth (cl-position choice labels :test #'string=) tunnels))
               (key (my/terminal--tunnel-key host sel)))
          (if (gethash key my/terminal--tunnel-process-table)
              (message "Tunnel already running: %s" choice)
            (let* ((cmd (my/terminal--build-tunnel-command host sel))
                   (proc (start-process (format "ssh-tunnel-%s" key)
                                        (get-buffer-create "*ssh-tunnels*")
                                        shell-file-name shell-command-switch cmd)))
              (set-process-query-on-exit-flag proc nil)
              (puthash key proc my/terminal--tunnel-process-table)
              (set-process-sentinel proc (lambda (p _e)
                                           (when (memq (process-status p) '(exit signal))
                                             (maphash (lambda (k v)
                                                        (when (eq v p) (remhash k my/terminal--tunnel-process-table)))
                                                      my/terminal--tunnel-process-table))))
              (message "Started tunnel: %s" choice))))))))
              

(defun my/terminal-tunnel-stop ()
  "Stop a running SSH tunnel."
  (interactive)
  (if (= (hash-table-count my/terminal--tunnel-process-table) 0)
      (message "No running tunnels")
    (let* ((keys (hash-table-keys my/terminal--tunnel-process-table))
           (choice (completing-read "Stop tunnel: " keys nil t))
           (proc (gethash choice my/terminal--tunnel-process-table)))
      (when (process-live-p proc)
        (delete-process proc))
      (remhash choice my/terminal--tunnel-process-table)
      (message "Stopped tunnel: %s" choice))))

(defun my/terminal-tunnel-list ()
  "List running tunnels in the echo area."
  (interactive)
  (if (= (hash-table-count my/terminal--tunnel-process-table) 0)
      (message "No running tunnels")
    (let ((keys (if (fboundp 'hash-table-keys)
                    (hash-table-keys my/terminal--tunnel-process-table)
                  (let (acc)
                    (maphash (lambda (k _v) (push k acc)) my/terminal--tunnel-process-table)
                    (nreverse acc)))))
      (message "%s" (string-join keys ", ")))))

;; ----------------------------------------------------------------------------
;; Transient menu
;; ----------------------------------------------------------------------------

(defun my/terminal-menu ()
  "Open terminal hub menu (loads transient on first use)."
  (interactive)
  (require 'transient)
  (unless (fboundp 'my/terminal-menu--impl)
    (transient-define-prefix my/terminal-menu--impl ()
      "Terminal hub"
      ["Terminal"
       ("t" "Open" my/terminal-open)
       ("T" "Open here" my/terminal-open-here)
       ("n" "Open named" my/terminal-open-named)]
      ["Hosts"
       ("h" "SSH connect" my/terminal-ssh-connect)
       ("d" "Remote Dired" my/terminal-remote-dired)
       ("e" "Edit hosts" (lambda () (interactive) (customize-variable 'my/terminal-hosts)))]
      ["Tunnels"
       ("s" "Start tunnel" my/terminal-tunnel-start)
       ("x" "Stop tunnel" my/terminal-tunnel-stop)
       ("l" "List tunnels" my/terminal-tunnel-list)]))
  (call-interactively 'my/terminal-menu--impl))

;; Keybindings are provided by the shared Command leader in setup-keys.el

;; ----------------------------------------------------------------------------
;; LM Studio command suggestions via setup-ai
;; ----------------------------------------------------------------------------

(defun my/terminal--send-to-vterm-or-insert (text &optional run)
  (if (and (boundp 'vterm--process) vterm--process)
      (progn
        (vterm-send-string text)
        (when run (vterm-send-return)))
    (insert text)
    (when run (insert "\n"))))

(defun my/terminal-ai-suggest (prompt)
  "Ask LM Studio for a shell command suggestion and insert it."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-string "Describe the command: "))))
  (let ((reply (my/ai-suggest-shell prompt)))
    (if (not reply)
        (message "LM Studio not reachable or no content returned.")
      (my/terminal--send-to-vterm-or-insert reply nil))))

(defun my/terminal-ai-suggest-and-run (prompt)
  "Ask LM Studio and run the suggested command in the terminal."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-string "Describe the command to run: "))))
  (let ((reply (my/ai-suggest-shell prompt)))
    (if (not reply)
        (message "LM Studio not reachable or no content returned.")
      (my/terminal--send-to-vterm-or-insert reply t))))

;; ----------------------------------------------------------------------------
;; Clipboard → vterm paste (simpleclip integration)
;; ----------------------------------------------------------------------------

(defun my/vterm-clipboard-yank ()
  "Paste system clipboard into vterm using simpleclip when available."
  (interactive)
  (let* ((text (cond
                ((fboundp 'simpleclip-get-contents)
                 (simpleclip-get-contents))
                ((fboundp 'gui-get-selection)
                 (gui-get-selection 'CLIPBOARD))
                (t nil))))
    (when (and text (not (string-empty-p text)))
      (if (and (boundp 'vterm--process) vterm--process)
          (vterm-send-string text t)
        (insert text)))))

(provide 'setup-terminal)
