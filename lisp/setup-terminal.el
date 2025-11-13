(add-hook 'vterm-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode -1)))

;; Core customizations and helpers for a Termius-like terminal workflow.

;; Dependencies used by this module
(require 'seq)
(require 'subr-x) ;; string-join, string-trim, string-remove-suffix
(require 'cl-lib)
(require 'tramp)
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
  (advice-add 'vterm--set-directory :after #'my/terminal--refresh-buffer-name))

(defun my/terminal--ensure-vterm ()
  "Ensure vterm is available. Return non-nil if ok."
  (and (eq my/terminal-backend 'vterm)
       (or (featurep 'vterm)
           (require 'vterm nil t))))

(defvar-local my/terminal--static-name nil
  "When non-nil, vterm buffer renaming is disabled for this buffer.")

(defun my/terminal--buffer-name (&optional directory)
  "Return canonical vterm buffer name for DIRECTORY.
When DIRECTORY is nil, fall back to `default-directory' or HOME."
  (let* ((raw (or directory default-directory "~"))
         (remote (file-remote-p raw))
         (path nil)
         (host nil))
    (if remote
        (let* ((vec (ignore-errors (tramp-dissect-file-name raw)))
               (local (or (and vec (tramp-file-name-localname vec)) raw))
               (remote-host (and vec (tramp-file-name-host vec))))
          (setq path (my/terminal--normalize-path local nil))
          (setq host (or (my/terminal--short-host remote-host)
                         (my/terminal--short-host (and (fboundp 'system-name)
                                                       (system-name))))))
      (setq path (my/terminal--normalize-path raw t))
      (setq host (my/terminal--short-host (and (fboundp 'system-name)
                                               (system-name)))))
    (format "vterm: %s@%s"
            (or path "~")
            (or host "localhost"))))

(defun my/terminal--normalize-path (path abbreviate)
  "Return PATH without trailing slash; abbreviate when ABBREVIATE is non-nil."
  (when path
    (let* ((as-dir (file-name-as-directory path))
           (clean (directory-file-name as-dir)))
      (unless (or (string-prefix-p "/" clean)
                  (string-prefix-p "~" clean))
        (setq clean (concat "/" clean)))
      (if abbreviate
          (abbreviate-file-name clean)
        clean))))

(defun my/terminal--short-host (host)
  "Return HOST without domain suffix; fallback to HOST when splitting fails."
  (when host
    (let ((short (car (split-string host "\\."))))
      (if (and short (> (length short) 0))
          short
        host))))

(defun my/terminal--rename-buffer (&optional directory)
  "Rename the current vterm buffer using DIRECTORY or `default-directory'."
  (when (and (derived-mode-p 'vterm-mode)
             (not (and (boundp 'my/terminal--static-name)
                       my/terminal--static-name)))
    (let* ((target (my/terminal--buffer-name directory))
           (current (buffer-name)))
      (unless (string-equal current target)
        (rename-buffer target t)))))

(defun my/terminal--refresh-buffer-name (&rest _)
  "Update vterm buffer name after OSC-7 directory sync."
  (my/terminal--rename-buffer))

(defun my/terminal-open ()
  "Open a new terminal session using the configured backend."
  (interactive)
  (cond
   ((and (eq my/terminal-backend 'vterm) (my/terminal--ensure-vterm))
    (let ((vterm-shell my/terminal-shell))
      (vterm (my/terminal--buffer-name))))
   (t (message "No supported terminal backend available."))))

(defun my/terminal-open-here (&optional directory)
  "Open a terminal in DIRECTORY or the current `default-directory'."
  (interactive)
  (let ((dir (or directory default-directory)))
    (cond
     ((and (eq my/terminal-backend 'vterm) (my/terminal--ensure-vterm))
      (let ((default-directory dir)
            (vterm-shell my/terminal-shell))
        (vterm (my/terminal--buffer-name dir))))
     (t (message "No supported terminal backend available.")))))

(defun my/terminal-open-named (name)
  "Open a terminal buffer with NAME using the configured backend."
  (interactive (let ((default (my/terminal--buffer-name)))
                 (list (read-string "Terminal name: " default nil nil default))))
  (cond
   ((and (eq my/terminal-backend 'vterm) (my/terminal--ensure-vterm))
    (let ((vterm-shell my/terminal-shell))
      (vterm name)))
   (t (message "No supported terminal backend available."))))

;; Find or create a vterm in the current frame, preferring the current buffer
;; if it's already a vterm. Otherwise reuse a visible vterm window in this
;; frame, or create a canonical name based on directory/hostname.

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

(defcustom my/terminal-hosts-ssh-config (expand-file-name "~/.ssh/config")
  "Path to the SSH config file used to derive `my/terminal-hosts'."
  :type 'file
  :group 'my-terminal)

(defcustom my/terminal-hosts-auto-refresh t
  "When non-nil, load host profiles from the SSH config during startup."
  :type 'boolean
  :group 'my-terminal)

(defun my/terminal--ssh-config-file ()
  "Return the absolute path to the SSH config used for host discovery."
  (and my/terminal-hosts-ssh-config
       (expand-file-name my/terminal-hosts-ssh-config)))

(defun my/terminal--ssh-config-clean-value (value)
  "Trim VALUE and strip surrounding quotes."
  (when value
    (let ((clean (string-trim value)))
      (setq clean (string-trim clean "\"" "\""))
      (setq clean (string-trim clean "'" "'"))
      (string-trim clean))))

(defun my/terminal--ssh-config-usable-host-name (name)
  "Return non-nil when NAME is meaningful (no wildcards or negation)."
  (let ((clean (my/terminal--ssh-config-clean-value name)))
    (and clean
         (not (string-empty-p clean))
         (not (string-match-p "[*?]" clean))
         (not (string-prefix-p "!" clean)))))

(defun my/terminal-refresh-hosts-from-ssh-config (&optional quiet)
  "Populate `my/terminal-hosts' from the configured SSH config file.
When QUIET is non-nil, suppress status messages."
  (interactive (list nil))
  (let ((file (my/terminal--ssh-config-file)))
    (cond
     ((not (and file (file-readable-p file)))
      (unless quiet
        (message "SSH config %s not readable" (or file "unset"))))
     ((not (require 'tramp nil t))
      (unless quiet
        (message "TRAMP not available; cannot read SSH hosts.")))
     (t
      (let* ((raw (tramp-parse-sconfig file))
             (hosts (seq-uniq
                     (delq nil
                           (mapcar
                            (lambda (entry)
                              (let* ((host (and (consp entry) (cadr entry)))
                                     (clean (my/terminal--ssh-config-clean-value host)))
                                (when (my/terminal--ssh-config-usable-host-name clean)
                                  (let* ((user (my/terminal--ssh-config-clean-value (car entry)))
                                         (plist (list :name clean :host clean)))
                                    (when (and user (not (string-empty-p user)))
                                      (setq plist (plist-put plist :user user)))
                                    plist))))
                            raw))
                     (lambda (a b)
                       (string-equal (plist-get a :name)
                                     (plist-get b :name))))))
        (if (null hosts)
            (unless quiet
              (message "No SSH hosts found in %s" file))
          (setq my/terminal-hosts hosts)
          (unless quiet
            (message "Loaded %d hosts from %s" (length hosts) file))))))))

(when my/terminal-hosts-auto-refresh
  (my/terminal-refresh-hosts-from-ssh-config t))

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
