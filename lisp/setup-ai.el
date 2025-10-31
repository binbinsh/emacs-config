;;; setup-ai.el --- Centralized AI helpers  -*- lexical-binding: t; -*-

;; Single source of truth for LM Studio client helpers, display utilities,
;; Emacs context collection, and the global `C-c a` AI entrypoint.

;;; Requirements

(require 'json)
(require 'url)
(require 'subr-x)
(require 'cl-lib)
(require 'project nil t)

;;; User-configurable variables are expected in `lisp/setup-profile.el`:
;; - my/lmstudio-base-url (e.g., "http://localhost:1234/v1")
;; - my/lmstudio-model (e.g., "gpt-oss-20b")
;; - my/lmstudio-timeout (seconds)

;;; Core LM Studio client

(defun my/ai--assert-config ()
  (unless (and (boundp 'my/lmstudio-base-url) my/lmstudio-base-url
               (boundp 'my/lmstudio-model) my/lmstudio-model)
    (user-error "Set my/lmstudio-base-url and my/lmstudio-model in lisp/setup-profile.el")))

(defun my/ai-strip-code-fences (text)
  (let ((s (or text "")))
    (when (and (string-prefix-p "```" s) (string-suffix-p "```" s))
      (setq s (string-trim s "```[a-zA-Z0-9_-]*\n" "```")))
    (string-trim s)))

(defun my/ai-chat (messages &optional timeout)
  "Send MESSAGES (alist list) to LM Studio and return assistant content string.
Optional TIMEOUT overrides `my/lmstudio-timeout'."
  (my/ai--assert-config)
  (let* ((endpoint (concat (string-remove-suffix "/" my/lmstudio-base-url) "/chat/completions"))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json; charset=utf-8")
                                      ("Accept" . "application/json")
                                      ("Accept-Charset" . "utf-8")))
         (url-request-timeout (or timeout my/lmstudio-timeout))
         (url-show-status nil)
         (url-request-coding-system 'utf-8)
         (payload (json-encode `((model . ,my/lmstudio-model)
                                 (messages . ,messages))))
         (url-request-data (encode-coding-string payload 'utf-8))
         (buf (url-retrieve-synchronously endpoint t t (or timeout my/lmstudio-timeout))))
    (unless buf (user-error "LM Studio not reachable"))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (unless (search-forward "\n\n" nil t)
            (when (boundp 'url-http-end-of-headers)
              (goto-char url-http-end-of-headers)))
          (let* ((status (and (boundp 'url-http-response-status) url-http-response-status)))
            (when (and status (>= status 400))
              (let* ((body (buffer-substring-no-properties (point) (point-max)))
                     (excerpt (substring body 0 (min 200 (length body)))))
                (user-error "LM Studio HTTP %s: %s" status (string-trim excerpt)))))
          (ignore-errors (decode-coding-region (point) (point-max) 'utf-8))
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol)
                 (resp (json-read))
                 (choices (alist-get 'choices resp))
                 (first (car choices))
                 (message (alist-get 'message first))
                 (content (alist-get 'content message)))
            content))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun my/ai-request-json (messages &optional timeout)
  "Send MESSAGES and parse assistant content as JSON.
Returns parsed elisp alist or nil if parsing fails."
  (let* ((content (my/ai-chat messages timeout))
         (str (my/ai-strip-code-fences content)))
    (when (and str (> (length str) 0))
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol))
        (condition-case _
            (json-read-from-string str)
          (error nil))))))

(defun my/ai-with-status (what thunk)
  "Display WHAT progress in minibuffer while calling THUNK."
  (let ((start (float-time)))
    (message "LM Studio: %s..." what)
    (prog1 (funcall thunk)
      (message "LM Studio: %s...done (%.1fs)" what (- (float-time) start)))))

;;; Display helpers

(defun my/ai--write-utf8-file (path content)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert content)))
  path)

(defun my/ai-display-markdown (title markdown)
  "Present Markdown content using markdown-mode when available; otherwise text-mode."
  (let* ((tmp (make-temp-file "lm-md-" nil ".md"))
         (buf nil))
    (my/ai--write-utf8-file tmp (or markdown ""))
    (setq buf (find-file-noselect tmp))
    (with-current-buffer buf
      (goto-char (point-min))
      (if (featurep 'markdown-mode)
          (markdown-mode)
        (text-mode))
      (setq buffer-file-coding-system 'utf-8-unix)
      (read-only-mode 1)
      (rename-buffer (format "*LM %s*" title) t))
    (pop-to-buffer buf)))

;;; Shell suggestion helper

(defun my/ai-suggest-shell (prompt)
  "Return a single concise shell command string for PROMPT, or nil."
  (let* ((messages `(((role . "system")
                      (content . "You are a CLI assistant. Output only one concise shell command without explanations."))
                     ((role . "user") (content . ,prompt))))
         (content (my/ai-with-status "suggesting command"
                                     (lambda () (my/ai-chat messages 15)))))
    (and (stringp content) (my/ai-strip-code-fences content))))

;;; Emacs context collection (bounded)

(defun my/ai--truncate (s maxlen)
  (let* ((str (or s ""))
         (len (length str)))
    (if (<= len maxlen)
        str
      (concat (substring str 0 maxlen) "\n... [truncated]"))))

(defun my/ai--window-snapshot (win)
  (with-selected-window win
    (let* ((buf (window-buffer win))
           (name (buffer-name buf))
           (file (buffer-file-name buf))
           (mode (format "%s" major-mode))
           (start (window-start win))
           (end (window-end win t))
           (visible (my/ai--truncate (buffer-substring-no-properties start end) 3000)))
      `((buffer_name . ,name)
        (file_path . ,file)
        (major_mode . ,mode)
        (visible_text . ,visible)))))

(defun my/ai--project-root ()
  (when (and (fboundp 'project-current) (project-current nil))
    (car (project-roots (project-current)))))

(defun my/ai-collect-context ()
  "Collect a compact snapshot of the current Emacs context as an alist."
  (let* ((wins (window-list (selected-frame) 'no-minibuf))
         (snapshots (mapcar #'my/ai--window-snapshot wins))
         (pt (point))
         (curline (line-number-at-pos pt))
         (curcol (current-column))
         (active (use-region-p))
         (region (and active (my/ai--truncate (buffer-substring-no-properties (region-beginning) (region-end)) 4000))))
    `((emacs_version . ,emacs-version)
      (system_type . ,(format "%s" system-type))
      (project_root . ,(my/ai--project-root))
      (cursor (line . ,curline) (column . ,curcol) (point . ,pt))
      (active_region . ,(and active t))
      (region_text . ,region)
      (windows . ,snapshots))))

;;; Global AI entrypoint bound to C-c a

(defun my/ai-execute (command)
  "Prompt for COMMAND, send with context to LM Studio, execute elisp or show info."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-string "AI command: "))))
  (let* ((ctx (my/ai-collect-context))
         (ctx-json (json-encode ctx))
         (sys (concat
               "You operate Emacs for the user. Always respond with strict JSON.\n"
               "Allowed schemas:\n"
               "- {\"type\":\"elisp\",\"elisp\":\"(progn (message \\\"ok\\\"))\"}\n"
               "- {\"type\":\"info\",\"title\":\"Title\",\"markdown\":\"...\"}"))
         (user (format "User command: %s\n\nContext(JSON): %s" command ctx-json))
         (messages `(((role . "system") (content . ,sys))
                     ((role . "user") (content . ,user))))
         (resp (my/ai-with-status "processing"
                                  (lambda () (my/ai-request-json messages my/lmstudio-timeout)))))
    (if (not resp)
        (let* ((fallback (my/ai-chat messages my/lmstudio-timeout)))
          (my/ai-display-markdown "AI" (or fallback "")))
      (let* ((type (alist-get 'type resp))
             (type (and type (downcase (format "%s" type)))))
        (cond
         ((string= type "elisp")
          (let* ((code (alist-get 'elisp resp)))
            (unless (and code (stringp code)) (user-error "Invalid elisp payload"))
            (eval (read code))
            (message "AI elisp executed.")))
         ((string= type "info")
          (let ((title (or (alist-get 'title resp) "AI"))
                (md (or (alist-get 'markdown resp) "")))
            (my/ai-display-markdown (format "%s" title) (format "%s" md))))
         (t
          (my/ai-display-markdown "AI" (json-encode resp))))))))

(provide 'setup-ai)

;;; setup-ai.el ends here


