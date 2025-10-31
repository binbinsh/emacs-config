;;; setup-gmail.el --- Gmail-like Notmuch + LM Studio integration -*- lexical-binding: t; -*-

(require 'use-package)
(require 'json)
(require 'url)
(require 'nerd-icons nil t)
(require 'xwidget nil t)
(require 'cl-lib)
(require 'subr-x)
(require 'mm-decode)
(require 'notmuch-lib)
(require 'url-util)
(require 'markdown-mode nil t)

;; Identities and LM Studio parameters should be defined in `lisp/setup-profile.el`:
;; - my/notmuch-identities   : list of "Full Name <[email protected]>"
;; - my/lmstudio-base-url    : string, e.g. "http://localhost:1234/v1"
;; - my/lmstudio-model       : string, e.g. "gpt-oss-120b"

;; Sending mail via lieer (gmi) wrapper
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program (expand-file-name "~/.local/bin/sendmail-gmi")
      message-sendmail-f-is-evil t
      message-sendmail-envelope-from 'header)

(when (boundp 'my/notmuch-identities)
  (setq notmuch-identities my/notmuch-identities))

(use-package notmuch
  :ensure t
  :commands (notmuch notmuch-search notmuch-show)
  :config
  ;; Prefer plain text in the notmuch buffer; do not inline-render HTML
  (with-eval-after-load 'mm-decode
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")
    (setq mm-inline-text/html nil))

  ;; Ensure xwidget is the default viewer inside notmuch buffers
  (add-hook 'notmuch-show-mode-hook
            (lambda ()
              (when (featurep 'xwidget-internal)
                (setq-local browse-url-browser-function #'xwidget-webkit-browse-url))))

  ;; Gmail-like saved searches (you can customize further)
  (setq notmuch-saved-searches
        '((:name "inbox"      :query "tag:inbox and not tag:trash and not tag:spam" :key "i")
          (:name "unread"     :query "tag:unread and tag:inbox" :key "u")
          (:name "starred"    :query "tag:flagged" :key "*")
          (:name "sent"       :query "tag:sent" :key "s")
          (:name "drafts"     :query "tag:draft" :key "d")
          (:name "all"        :query "*" :key "a")
          (:name "spam"       :query "tag:spam" :key "!s")
          (:name "trash"      :query "tag:trash" :key "!t")
          ;; If Gmail categories sync as tags (may vary by account setup)
          (:name "primary"    :query "tag:category:primary" :key "gp")
          (:name "social"     :query "tag:category:social" :key "gs")
          (:name "updates"    :query "tag:category:updates" :key "gu")
          (:name "forums"     :query "tag:category:forums" :key "gf")
          (:name "promotions" :query "tag:category:promotions" :key "gm"))))

;; 3-pane Gmail-like layout and helpers
(defvar my/notmuch-ui-left-window nil)
(defvar my/notmuch-ui-top-right-window nil)
(defvar my/notmuch-ui-bottom-right-window nil)

;; Remember last visited message/thread ids (useful when WebKit view replaces show buffer)
(defvar my/notmuch-last-message-id nil)
(defvar my/notmuch-last-thread-id nil)

(defvar my/notmuch-left-width 42
  "Desired width (in columns) for the left pane.")

(defvar my/notmuch-left-ignored-tags
  '("inbox" "unread" "sent" "draft" "trash" "spam" "flagged"
    "attachment" "signed" "encrypted")
  "Tags to hide from the left Labels list.")

(defvar my/notmuch-use-icons t
  "Whether to show Nerd Font icons in the left pane.")

(defvar my/notmuch-default-webkit t
  "If non-nil, render message content in the bottom-right using WebKit by default.")

(defun my/notmuch--title-case (s)
  (let* ((clean (replace-regexp-in-string "[_-]+" " " (format "%s" s))))
    (mapconcat #'capitalize (split-string clean "[[:space:]]+" t) " ")))

(defun my/notmuch--capitalize-first (s)
  (if (and s (> (length s) 0))
      (concat (upcase (substring s 0 1)) (substring s 1))
    s))

(defun my/notmuch--icon-for-saved (name)
  (when (and my/notmuch-use-icons (featurep 'nerd-icons))
    (let ((n (downcase (format "%s" name))))
      (cond
       ((string= n "inbox")      (nerd-icons-mdicon "nf-md-inbox"))
       ((string= n "unread")     (nerd-icons-mdicon "nf-md-email"))
       ((string= n "starred")    (nerd-icons-mdicon "nf-md-star"))
       ((string= n "sent")       (nerd-icons-mdicon "nf-md-send"))
       ((string= n "drafts")     (nerd-icons-mdicon "nf-md-file_document_outline"))
       ((string= n "all")        (nerd-icons-mdicon "nf-md-email_multiple"))
       ((string= n "spam")       (nerd-icons-mdicon "nf-md-alert_octagon"))
       ((string= n "trash")      (nerd-icons-mdicon "nf-md-delete"))
       ((string= n "primary")    (nerd-icons-mdicon "nf-md-inbox"))
       ((string= n "social")     (nerd-icons-mdicon "nf-md-account_group"))
       ((string= n "updates")    (nerd-icons-mdicon "nf-md-update"))
       ((string= n "forums")     (nerd-icons-mdicon "nf-md-forum"))
       ((string= n "promotions") (nerd-icons-mdicon "nf-md-tag"))
       (t (nerd-icons-mdicon "nf-md-magnify"))))))

(defun my/notmuch--icon-for-label (_tag)
  (when (and my/notmuch-use-icons (featurep 'nerd-icons))
    (nerd-icons-mdicon "nf-md-tag")))

(defun my/notmuch--open-search-in-top-right (query)
  (unless (window-live-p my/notmuch-ui-top-right-window)
    (user-error "Notmuch Gmail UI is not initialized; run C-c m"))
  (with-selected-window my/notmuch-ui-top-right-window
    (notmuch-search query)
    (my/notmuch--ensure-bottom-window)
    ;; Move to first result if possible, then open via notmuch built-in to trigger advice
    (goto-char (point-min))
    (when (fboundp 'notmuch-search-show-thread)
      (notmuch-search-show-thread))
    (select-window my/notmuch-ui-top-right-window)))

(defun my/notmuch--bottom-right-window ()
  (or (and (window-live-p my/notmuch-ui-top-right-window)
           (or (window-in-direction 'below my/notmuch-ui-top-right-window)
               (with-selected-window my/notmuch-ui-top-right-window
                 (split-window-below))))
      (and (window-live-p my/notmuch-ui-bottom-right-window)
           my/notmuch-ui-bottom-right-window)))

;; Route any notmuch-show to bottom-right window reliably
(defun my/notmuch--advice-show (orig-fun &rest args)
  (let ((target (my/notmuch--bottom-right-window)))
    (if (window-live-p target)
        (with-selected-window target
          (apply orig-fun args)
          ;; Capture ids from the freshly opened notmuch-show buffer before switching to WebKit
          (when (derived-mode-p 'notmuch-show-mode)
            (setq my/notmuch-last-message-id (and (fboundp 'notmuch-show-get-message-id)
                                                  (notmuch-show-get-message-id)))
            (setq my/notmuch-last-thread-id (and (fboundp 'notmuch-show-get-thread-id)
                                                 (ignore-errors (notmuch-show-get-thread-id)))))
          (when (and my/notmuch-default-webkit
                     (featurep 'xwidget-internal)
                     (fboundp 'xwidget-webkit-browse-url))
            (my/notmuch-show-html-in-webkit))
          (when (window-live-p my/notmuch-ui-top-right-window)
            (select-window my/notmuch-ui-top-right-window)))
      (apply orig-fun args))))

(with-eval-after-load 'notmuch
  (advice-add 'notmuch-show :around #'my/notmuch--advice-show))

(defun my/notmuch--collect-tags ()
  "Return a list of tags by calling notmuch (includes all)."
  (let ((buf (generate-new-buffer " *notmuch-tags*"))
        (tags '()))
    (unwind-protect
        (when (zerop (call-process "notmuch" nil buf nil "search" "--output=tags" "--exclude=false" "*"))
          (with-current-buffer buf
            (goto-char (point-min))
            (while (not (eobp))
              (let ((tagline (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                (unless (string-empty-p tagline) (push tagline tags)))
              (forward-line 1))))
      (kill-buffer buf))
    (nreverse tags)))

(defface my/notmuch-left-label-face
  '((t :inherit default :underline nil))
  "Face for left pane labels without underline.")

(defface my/notmuch-left-saved-face
  '((t :inherit default :underline nil))
  "Face for left pane saved searches without underline.")

(defface my/notmuch-left-header-face
  '((t :inherit default :weight bold))
  "Face for left pane section headers.")

(define-derived-mode my/notmuch-left-mode special-mode "Notmuch-Left"
  "Simple left pane listing saved searches and tags.")

(defun my/notmuch--insert-left-pane ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Saved Searches" 'face 'my/notmuch-left-header-face) "\n")
    (dolist (s notmuch-saved-searches)
      (let* ((name (plist-get s :name))
             (query (plist-get s :query)))
        (insert-text-button (format "  %s %s"
                                    (or (my/notmuch--icon-for-saved name) "•")
                                    (my/notmuch--title-case name))
                            'follow-link t
                            'help-echo query
                            'face 'my/notmuch-left-saved-face
                            'action (lambda (_)
                                      (my/notmuch--open-search-in-top-right query)))
        (insert "\n")))
    (insert "\n" (propertize "Labels" 'face 'my/notmuch-left-header-face) "\n")
    (dolist (tag (my/notmuch--collect-tags))
      (unless (or (member tag my/notmuch-left-ignored-tags)
                  (string-prefix-p "category:" tag))
        (let* ((q (format "tag:%s and not tag:trash and not tag:spam" tag))
               (label (my/notmuch--capitalize-first tag)))
          (insert-text-button (format "  %s %s"
                                      (or (my/notmuch--icon-for-label tag) "🏷")
                                      label)
                              'follow-link t
                              'help-echo q
                              'face 'my/notmuch-left-label-face
                              'action (lambda (_)
                                        (my/notmuch--open-search-in-top-right q)))
          (insert "\n"))))
    (goto-char (point-min))))

(defun my/notmuch-open-gmail-ui ()
  "Open a 3-pane notmuch layout: left labels, top-right search, bottom-right thread."
  (interactive)
  (require 'notmuch)
  (delete-other-windows)
  ;; Left
  (setq my/notmuch-ui-left-window (selected-window))
  (let ((buf (get-buffer-create "*notmuch-left*")))
    (with-current-buffer buf
      (my/notmuch-left-mode)
      (my/notmuch--insert-left-pane))
    (switch-to-buffer buf))
  ;; Right split
  (setq my/notmuch-ui-top-right-window (split-window-right))
  ;; Resize left pane to desired width
  (let* ((curw (window-total-width my/notmuch-ui-left-window))
         (delta (- my/notmuch-left-width curw)))
    (when (/= delta 0)
      (window-resize my/notmuch-ui-left-window delta t)))
  (select-window my/notmuch-ui-top-right-window)
  ;; Bottom-right
  (setq my/notmuch-ui-bottom-right-window (split-window-below))
  ;; Start with inbox in top-right and first thread below
  (my/notmuch--open-search-in-top-right "tag:inbox and not tag:trash and not tag:spam")
  ;; Return focus to top-right
  (select-window my/notmuch-ui-top-right-window))

(defun my/notmuch--ensure-bottom-window ()
  "Ensure bottom-right window exists and is stored."
  (setq my/notmuch-ui-bottom-right-window (my/notmuch--bottom-right-window)))

;; Inline cid: images as data: URIs for xwidget-webkit
(defun my/notmuch--inline-cid-data-uris (html)
  "Replace cid: URLs in HTML with data: URIs using notmuch's CID registry."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (while (re-search-forward "\\(src\\|href\\)=[\\\"']cid:([^\\\"']+)[\\\"']" nil t)
      (let* ((attr (match-string 1))
             (enc (match-string 2))
             (cid (url-unhex-string enc))
             (content-and-type (ignore-errors (notmuch-show--get-cid-content cid)))
             (bytes (and content-and-type (car content-and-type)))
             (ctype (and content-and-type (cadr content-and-type)))
             (data (and bytes ctype (concat "data:" ctype ";base64," (base64-encode-string bytes t)))))
        (when data
          (replace-match (concat attr "=\"" data "\"") t t))))
    (buffer-string)))

;; Find the first text/html MIME part from a notmuch BODY tree
(defun my/notmuch--find-html-part (body)
  (cl-labels
      ((walk (part)
         (let* ((ctype (plist-get part :content-type))
                (ctype (and ctype (downcase ctype))))
           (cond
            ((and ctype (string= ctype "text/html")) part)
            ((and ctype (string-prefix-p "multipart/" ctype))
             (cl-loop for p in (plist-get part :content)
                      for r = (walk p)
                      when r return r))
            ((and ctype (string= ctype "message/rfc822"))
             (let* ((inner-msg (car (plist-get part :content)))
                    (inner-body (plist-get inner-msg :body)))
               (when inner-body
                 (cl-loop for p in inner-body
                          for r = (walk p)
                          when r return r))))
            (t nil)))))
    (and (listp body)
         (cl-loop for p in body
                  for r = (walk p)
                  when r return r))))

;; HTML rendering in WebKit (xwidget)
(defun my/notmuch-show-html-in-webkit ()
  "Open current message HTML part in an xwidget WebKit buffer."
  (interactive)
  (unless (featurep 'xwidget-internal)
    (user-error "This Emacs was not built with xwidgets/WebKit support"))
  (unless (derived-mode-p 'notmuch-show-mode)
    (user-error "Open a message in notmuch-show first"))
  (let* ((msg (notmuch-show-get-message-properties))
         (body (plist-get msg :body))
         (process-crypto (and (boundp 'notmuch-show-process-crypto)
                              notmuch-show-process-crypto)))
    (let ((html-part (my/notmuch--find-html-part body)))
      (unless html-part
        (user-error "No HTML part found in this message"))
      (let* ((html-raw (notmuch-get-bodypart-text msg html-part process-crypto 'cache))
             (html (my/notmuch--inline-cid-data-uris html-raw))
             (html (my/notmuch--ensure-utf8-meta html))
             (tmp (make-temp-file "notmuch-" nil ".html")))
        (my/notmuch--write-utf8-file tmp html)
        (xwidget-webkit-browse-url (concat "file://" tmp))))))

;; LM Studio: minimal OpenAI-compatible chat/completions client
(defun my/lmstudio--assert-config ()
  (unless (and (boundp 'my/lmstudio-base-url) my/lmstudio-base-url
               (boundp 'my/lmstudio-model) my/lmstudio-model)
    (user-error "Set my/lmstudio-base-url and my/lmstudio-model in lisp/setup-profile.el")))

(defun my/lmstudio-chat (messages &optional timeout)
  "Send MESSAGES (alist list) to LM Studio and return assistant content string.
Optional TIMEOUT (seconds) overrides `my/lmstudio-timeout'."
  (my/lmstudio--assert-config)
  (let* ((endpoint (concat (string-remove-suffix "/" my/lmstudio-base-url) "/chat/completions"))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")
                                      ("Accept" . "application/json")
                                      ("Accept-Charset" . "utf-8")))
         (url-request-timeout (or timeout my/lmstudio-timeout))
         (url-show-status nil)
         (url-request-data (json-encode `((model . ,my/lmstudio-model)
                                          (messages . ,messages))))
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
          ;; Force UTF-8 decode of the JSON body regardless of headers
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

;; Echo-area status helper for LM Studio calls
(defun my/lmstudio--with-echo-status (what thunk)
  "Display WHAT progress in minibuffer while calling THUNK."
  (let ((start (float-time)))
    (message "LM Studio: %s..." what)
    (prog1 (funcall thunk)
      (message "LM Studio: %s...done (%.1fs)" what (- (float-time) start)))))

(defun my/notmuch--current-message-id ()
  "Return the current message id at point from show/tree/search contexts.
Prefers the exact message under point when available."
  (cond
   ;; Current buffer: show → precise message id
   ((derived-mode-p 'notmuch-show-mode)
    (and (fboundp 'notmuch-show-get-message-id)
         (notmuch-show-get-message-id)))
   ;; Current buffer: tree → precise message id under cursor
   ((and (fboundp 'notmuch-tree-get-message-id)
         (derived-mode-p 'notmuch-tree-mode))
    (notmuch-tree-get-message-id))
   ;; Current buffer: search → fall back to first message of the thread at point
   ((derived-mode-p 'notmuch-search-mode)
    (let ((tid (and (fboundp 'notmuch-search-find-thread-id)
                    (notmuch-search-find-thread-id))))
      (my/notmuch--thread-first-message-id tid)))
   ;; Bottom-right window context
   ((and (window-live-p my/notmuch-ui-bottom-right-window))
    (with-selected-window my/notmuch-ui-bottom-right-window
      (cond
       ((derived-mode-p 'notmuch-show-mode)
        (and (fboundp 'notmuch-show-get-message-id)
             (notmuch-show-get-message-id)))
       ((and (fboundp 'notmuch-tree-get-message-id)
             (derived-mode-p 'notmuch-tree-mode))
        (notmuch-tree-get-message-id)))))
   ;; Top-right window context
   ((and (window-live-p my/notmuch-ui-top-right-window))
    (with-selected-window my/notmuch-ui-top-right-window
      (cond
       ((derived-mode-p 'notmuch-search-mode)
        (let ((tid (and (fboundp 'notmuch-search-find-thread-id)
                        (notmuch-search-find-thread-id))))
          (my/notmuch--thread-first-message-id tid)))
       ((and (fboundp 'notmuch-tree-get-message-id)
             (derived-mode-p 'notmuch-tree-mode))
        (notmuch-tree-get-message-id)))))
   ;; Fallback to last captured id from advice
   (t my/notmuch-last-message-id)))

(defun my/notmuch--current-thread-id ()
  "Return the current thread id from notmuch context if available."
  (cond
   ((derived-mode-p 'notmuch-show-mode)
    (and (fboundp 'notmuch-show-get-thread-id)
         (ignore-errors (notmuch-show-get-thread-id))))
   ((derived-mode-p 'notmuch-search-mode)
    (and (fboundp 'notmuch-search-find-thread-id)
         (notmuch-search-find-thread-id)))
   ((and (window-live-p my/notmuch-ui-bottom-right-window))
    (with-selected-window my/notmuch-ui-bottom-right-window
      (when (derived-mode-p 'notmuch-show-mode)
        (and (fboundp 'notmuch-show-get-thread-id)
             (ignore-errors (notmuch-show-get-thread-id))))))
   ((and (window-live-p my/notmuch-ui-top-right-window))
    (with-selected-window my/notmuch-ui-top-right-window
      (when (derived-mode-p 'notmuch-search-mode)
        (and (fboundp 'notmuch-search-find-thread-id)
             (notmuch-search-find-thread-id)))))
   (t my/notmuch-last-thread-id)))

(defun my/notmuch--thread-first-message-id (thread-id)
  "Return the first message id (\"id:...\") of THREAD-ID using notmuch search."
  (when thread-id
    (with-temp-buffer
      (let ((exit (call-process "notmuch" nil (current-buffer) nil
                                "search" "--output=messages" (concat "thread:" thread-id))))
        (when (zerop exit)
          (goto-char (point-min))
          (when (re-search-forward "^id:[^\n]+" nil t)
            (match-string 0)))))))

;; Normalize various id forms into notmuch CLI query strings
(defun my/notmuch--ensure-id-query (maybe-id)
  "Return a notmuch id: query string from MAYBE-ID.
Accepts either a bare message id or an \"id:...\" string."
  (let ((s (and maybe-id (format "%s" maybe-id))))
    (unless (and s (> (length s) 0))
      (user-error "No current message selected; open a message first"))
    (if (string-prefix-p "id:" s) s (concat "id:" s))))

(defun my/notmuch--ensure-thread-query ()
  "Return a notmuch thread: query string for the current context.
Falls back to resolving from current message id when needed."
  (let ((tid (my/notmuch--current-thread-id)))
    (unless tid
      (let* ((mid (or (my/notmuch--current-message-id)
                      (let ((t2 (my/notmuch--current-thread-id)))
                        (my/notmuch--thread-first-message-id t2))))
             (idq (my/notmuch--ensure-id-query mid)))
        (when idq
          (with-temp-buffer
            (let ((exit (call-process "notmuch" nil (current-buffer) nil
                                      "search" "--output=threads" idq)))
              (when (zerop exit)
                (goto-char (point-min))
                (when (re-search-forward "^thread:[^\n]+" nil t)
                  (setq tid (match-string 0)))))))))
    (unless tid (user-error "No current thread selected; open a thread first"))
    tid))

(defun my/notmuch--current-message-raw ()
  "Return raw RFC822 text for the current message.
Prefers the message shown in bottom-right notmuch-show; if unavailable, opens the thread from the top-right pane and uses its first message."
  (let* ((msg-id (or (my/notmuch--current-message-id)
                     (let ((thread-id (my/notmuch--current-thread-id)))
                       (my/notmuch--thread-first-message-id thread-id))))
         (idq (my/notmuch--ensure-id-query msg-id)))
    (with-temp-buffer
      (let ((exit (call-process "notmuch" nil (cons (current-buffer) t) nil
                                "show" "--format=raw" idq)))
        (if (zerop exit)
            (buffer-string)
          (let ((excerpt (buffer-substring-no-properties
                          (point-min)
                          (min (point-max) (+ (point-min) 400)))))
            (user-error "notmuch CLI failed (exit %s): %s" exit (string-trim excerpt))))))))

(defun my/notmuch--current-thread-raw ()
  "Return raw RFC822 text for the current thread."
  (let ((threadq (my/notmuch--ensure-thread-query)))
    (with-temp-buffer
      (let ((exit (call-process "notmuch" nil (cons (current-buffer) t) nil
                                "show" "--format=raw" threadq)))
        (if (zerop exit)
            (buffer-string)
          (let ((excerpt (buffer-substring-no-properties
                          (point-min)
                          (min (point-max) (+ (point-min) 400)))))
            (user-error "notmuch CLI failed (exit %s): %s" exit (string-trim excerpt))))))))

;; Ensure UTF-8 meta is present in HTML so browsers/WebKit decode correctly
(defun my/notmuch--ensure-utf8-meta (html)
  (with-temp-buffer
    (let ((case-fold-search t))
      (insert html)
      (goto-char (point-min))
      ;; Normalize <meta charset=...>
      (while (re-search-forward "<meta[^>]*charset=\([\"']\)?[^\"' />]+\1?[^>]*>" nil t)
        (replace-match "<meta charset=\"utf-8\">" t t))
      ;; Normalize <meta http-equiv="Content-Type" ...>
      (goto-char (point-min))
      (while (re-search-forward "<meta[^>]*http-equiv=\([\"']\)?content-type\1?[^>]*>" nil t)
        (let* ((tag (match-string 0))
               (fixed (if (string-match "content=\([\"']\)[^\"']*\1" tag)
                          (replace-regexp-in-string
                           "charset=[^\"';> ]+" "charset=utf-8" tag t t)
                        "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">")))
          (replace-match fixed t t)))
      ;; Insert a charset if missing
      (let ((has-charset (progn (goto-char (point-min))
                                (re-search-forward "<meta[^>]*charset" nil t))))
        (unless has-charset
          (goto-char (point-min))
          (if (re-search-forward "<head[^>]*>" nil t)
              (insert "\n<meta charset=\"utf-8\">\n")
            (progn
              (goto-char (point-min))
              (insert "<meta charset=\"utf-8\">\n")))))
      (buffer-string))))

;; Write text content to PATH using utf-8-unix coding
(defun my/notmuch--write-utf8-file (path content)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert content))))

;; Present Markdown content using markdown-mode when available; otherwise text-mode
(defun my/notmuch--display-markdown (title markdown)
  (let* ((tmp (make-temp-file "lm-summary-" nil ".md"))
         (buf nil))
    (my/notmuch--write-utf8-file tmp markdown)
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


(defun my/notmuch-ai-suggest-labels ()
  "Ask LM Studio to suggest JSON array of tags for the current message."
  (interactive)
  (let* ((msg-id (or (my/notmuch--current-message-id)
                     (let ((tid (my/notmuch--current-thread-id))) (my/notmuch--thread-first-message-id tid))))
         (idq (my/notmuch--ensure-id-query msg-id))
         (all-tags (my/notmuch--collect-tags))
         (allowed-tags (cl-remove-if (lambda (tag)
                                       (or (member tag my/notmuch-left-ignored-tags)
                                           (string-prefix-p "category:" tag)))
                                     all-tags))
         (allowed-lc (mapcar (lambda (t) (downcase (format "%s" t))) allowed-tags))
         (allowed-map (let (alist)
                        (dolist (t allowed-tags alist)
                          (push (cons (downcase (format "%s" t)) t) alist))))
         (allowed-json (concat "[" (mapconcat (lambda (s) (format "\"%s\"" s)) allowed-lc ", ") "]"))
         (raw (my/notmuch--current-message-raw))
         (prompt (concat
                  "You are tagging emails for notmuch.\n"
                  "Only choose tags from the ALLOWED list below.\n"
                  "Respond ONLY with a JSON array (1-5 items) of lowercase tags from that list.\n"
                  "If none apply, return [].\n"
                  "ALLOWED: " allowed-json "\n\n"
                  raw))
         (content (my/lmstudio--with-echo-status
                   "suggesting labels"
                   (lambda () (my/lmstudio-chat `(((role . "user") (content . ,prompt))) 15))))
         (tags (condition-case nil
                   (json-read-from-string content)
                 (error (user-error "Model did not return valid JSON: %s" content)))))
    (unless msg-id (user-error "No current message selected; open a message first"))
    (when (not (listp tags)) (user-error "Expected JSON array"))
    (let* ((sanitized (delete-dups
                       (delq nil
                            (mapcar (lambda (tag)
                                      (let* ((s (downcase (format "%s" tag)))
                                             (s (replace-regexp-in-string "[\\s/]+" "-" s))
                                             (s (replace-regexp-in-string "[^a-z0-9:._-]" "" s)))
                                        (and (> (length s) 0) s)))
                                    tags))))
           (sanitized (cl-remove-if-not (lambda (s) (member s allowed-lc)) sanitized))
           (canonical (mapcar (lambda (s) (cdr (assoc s allowed-map))) sanitized))
           (tag-args (mapcar (lambda (s) (concat "+" s)) canonical)))
      (if (null canonical)
          (message "No allowed tags suggested; nothing applied")
        ;; Apply tags via CLI for robustness
        (apply #'call-process "notmuch" nil nil nil
               (append (list "tag") tag-args (list idq)))
        (message "Applied tags: %s" canonical)))))

(defun my/notmuch-ai-generate-reply ()
  "Insert a reply draft generated by LM Studio into the current message buffer."
  (interactive)
  (let* ((raw (my/notmuch--current-message-raw))
         (prompt (concat
                  "Write a concise, polite reply email to the following message.\n"
                  "Return only the email body text.\n\n"
                  raw))
         (content (my/lmstudio--with-echo-status
                   "generating reply"
                   (lambda () (my/lmstudio-chat `(((role . "user") (content . ,prompt))) my/lmstudio-timeout)))))
    (with-current-buffer (current-buffer)
      (when (derived-mode-p 'message-mode)
        (goto-char (point-max))
        (insert "\n\n" content)))))

(defun my/notmuch-ai-summarize-thread ()
  "Summarize the current message if available; otherwise the whole thread."
  (interactive)
  (let* ((mid (my/notmuch--current-message-id))
         (raw (if mid
                  (with-temp-buffer
                    (let* ((idq (my/notmuch--ensure-id-query mid))
                           (exit (call-process "notmuch" nil (cons (current-buffer) t) nil
                                               "show" "--format=raw" idq)))
                      (if (zerop exit)
                          (buffer-string)
                        (let ((excerpt (buffer-substring-no-properties
                                        (point-min)
                                        (min (point-max) (+ (point-min) 400)))))
                          (user-error "notmuch CLI failed (exit %s): %s" exit (string-trim excerpt))))))
                (my/notmuch--current-thread-raw)))
         (prompt (if mid
                     (concat "Summarize the following email in English with concise bullet points and action items.\n\n" raw)
                   (concat "Summarize the email thread below with bullet points and action items in English.\n\n" raw)))
         (content (my/lmstudio--with-echo-status
                   "summarizing thread"
                   (lambda () (my/lmstudio-chat `(((role . "user") (content . ,prompt))) my/lmstudio-timeout)))))
    (my/notmuch--display-markdown "Summary" content)))

(provide 'setup-gmail)


