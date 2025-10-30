;;; setup-snippets.el --- Snippet selection and expansion setup
;;
;; Provides:
;; - Yasnippet with global activation and local snippet dir
;; - Consult-powered global selector (routes sh-mode snippets to vterm)
;;
;;; Code:

(require 'seq)
(require 'subr-x)

(use-package yasnippet
  :init
  (let* ((private (expand-file-name "emacs/snippets" (expand-file-name "~/.config")))
         (repo (expand-file-name "snippets" user-emacs-directory))
         (dirs (delq nil (list private (when (file-directory-p repo) repo)))))
    (unless (file-directory-p private)
      (make-directory private t))
    (setq yas-snippet-dirs dirs))
  :config
  (yas-global-mode 1))


;; Global smart selector: choose any snippet; route shell snippets to vterm

(defun my/snippet--parse-header-line (line)
  (when (string-match "^#\\s-+\\([^:]+\\):\\s-*\\(.*\\)$" line)
    (cons (downcase (match-string 1 line)) (match-string 2 line))))

(defun my/snippet--read-header (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((hdr (make-hash-table :test 'equal))
          (done nil))
      (while (and (not done) (not (eobp)))
        (let ((l (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (cond
           ((string-prefix-p "# --" l) (setq done t))
           ((string-prefix-p "#" l)
            (let ((kv (my/snippet--parse-header-line l)))
              (when kv (puthash (car kv) (cdr kv) hdr))))))
        (forward-line 1))
      (list :name (or (gethash "name" hdr) (file-name-nondirectory file))
            :key (gethash "key" hdr)
            :group (gethash "group" hdr)))))

(defun my/snippet--format-display (modestr group key name)
  (format "[%s]%s%s - %s"
          (or modestr "-")
          (if (string-empty-p group) "" (concat " " group))
          (if (string-empty-p key)   "" (concat " " key))
          name))

(defun my/snippet--collect-all ()
  "Return list of (DISPLAY . DATA). DATA plists include :file :mode :name :key :group."
  (let (acc)
    (dolist (root yas-snippet-dirs)
      (when (and root (file-directory-p root))
        (dolist (file (directory-files-recursively root "[^/]") )
          (when (file-regular-p file)
            (let* ((rel (file-relative-name file root)))
              (unless (string-match-p "/\\." rel)
                (let* ((parts (split-string rel "/" t))
                       (modestr (car parts))
                       (mode (and modestr (intern modestr)))
                       (meta (my/snippet--read-header file))
                       (name (plist-get meta :name))
                       (key (or (plist-get meta :key) ""))
                       (group (or (plist-get meta :group) ""))
                       (disp (my/snippet--format-display modestr group key name))
                       (data (list :file file :mode mode :name name :key key :group group)))
                  (push (cons disp data) acc))))))))
    (nreverse acc)))

(defun my/snippet--shell-mode-p (mode)
  (memq mode '(sh-mode shell-script-mode bash-ts-mode bash-mode)))

(defun my/snippet--read-body (file)
  "Return the body (after `# --`) of a yasnippet FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^# --\\s-*$" nil t)
      (forward-line 1)
      (buffer-substring-no-properties (point) (point-max)))))

;; Disable Yasnippet in vterm buffers to avoid scanning non-existent mode dirs
(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook (lambda () (yas-minor-mode -1))))

(defun my/snippet--ensure-local-vterm ()
  "Return a vterm buffer; prefer current/visible/MRU, else create locally."
  (when (or (featurep 'vterm) (require 'vterm nil t))
    (cond
     ((derived-mode-p 'vterm-mode) (current-buffer))
     (t
      (let ((visible
             (seq-find (lambda (b)
                         (with-current-buffer b (derived-mode-p 'vterm-mode)))
                       (mapcar #'window-buffer (window-list))))
            (mru
             (seq-find (lambda (b)
                         (with-current-buffer b (derived-mode-p 'vterm-mode)))
                       (buffer-list))))
        (or visible
            mru
            (let ((default-directory (expand-file-name "~")))
              (vterm)
              (current-buffer))))))))

(defun my/snippet--vterm-send (text)
  "Send TEXT to a vterm buffer.
Trims trailing newlines and uses bracketed paste when multi-line."
  (when (and (stringp text) (> (length text) 0))
    (let ((buf (my/snippet--ensure-local-vterm)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let* ((clean (string-trim-right text "[\n\r]+"))
                 (multi (string-match-p "\n" clean)))
            (vterm-send-string clean multi)))
        (pop-to-buffer buf)))))

(defun my/snippet-expand-to-string-by (file mode)
  "Expand snippet FILE using MODE in a temp buffer and return text."
  (let* ((tmp (generate-new-buffer " *yas-tmp*"))
         (body (my/snippet--read-body file))
         (result ""))
    (unless (and (stringp body) (> (length body) 0))
      (user-error "Empty snippet body: %s" file))
    (unwind-protect
        (with-current-buffer tmp
          (funcall mode)
          (yas-minor-mode 1)
          (yas-expand-snippet body)
          (setq result (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer tmp))
    result))

(defun my/snippet-select-smart ()
  "Global snippet selector.
Shell snippets: open local vterm, paste and run.
Other snippets: expand at point (or paste into vterm)."
  (interactive)
  (let* ((cands (my/snippet--collect-all))
         (choice (cond
                  ((null cands)
                   (user-error "No snippets found in yas-snippet-dirs"))
                  ((fboundp 'consult--read)
                   (consult--read (mapcar #'car cands) :prompt "Snippet (global): " :require-match t))
                  (t
                   (completing-read "Snippet (global): " (mapcar #'car cands) nil t))))
         (data (assoc choice cands))
         (file (plist-get (cdr data) :file))
         (mode (plist-get (cdr data) :mode))
         (name (plist-get (cdr data) :name)))
    (if (my/snippet--shell-mode-p mode)
        (let ((txt (my/snippet-expand-to-string-by file (or mode 'sh-mode))))
          (if (not (featurep 'vterm))
              (user-error "vterm not available for shell snippets")
            (my/snippet--vterm-send txt)))
      (let ((body (and file (my/snippet--read-body file))))
        (if (and (stringp body) (> (length body) 0))
            (if (derived-mode-p 'vterm-mode)
                (my/snippet--vterm-send body)
              (yas-minor-mode 1)
              (yas-expand-snippet body))
          (message "Snippet body not found or empty: %s" name))))))

(provide 'setup-snippets)

;;; setup-snippets.el ends here


