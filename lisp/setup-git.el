;;; setup-git.el --- Fork-like Git UX with Magit and AI commits -*- lexical-binding: t; -*-

;; This file wires a Fork-like Git workflow inside Emacs using Magit.
;; It provides: quick status, blame toggle, file history, branch graph,
;; manual repo list management, and an AI commit message generator that
;; can call LM Studio (local OpenAI-compatible) or an online OpenAI-compatible API.

(use-package magit
  :commands (magit-status magit-status-setup-buffer magit-dispatch magit-file-dispatch
             magit-blame-addition magit-blame-quit magit-log-buffer-file magit-log-all)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-bury-buffer-function #'magit-restore-window-configuration))

(require 'subr-x)  ; for string-empty-p
(require 'project nil t) ; for project-current/project-root (optional at startup)

;; -----------------------------------------------------------------------------
;; Customization
;; -----------------------------------------------------------------------------

(defgroup fork-git nil
  "Fork-like Git UX in Emacs."
  :group 'tools)

(defcustom fork-git-repositories nil
  "List of absolute directories of known Git repositories."
  :type '(repeat directory))

(defcustom fork-git-ai-backend 'lmstudio
  "AI backend for commit messages.
One of 'lmstudio (local LM Studio OpenAI-compatible) or 'openai (online OpenAI-compatible)."
  :type '(choice (const :tag "LM Studio" lmstudio)
                 (const :tag "OpenAI-compatible" openai)))

(defcustom fork-git-lmstudio-endpoint "http://localhost:1234/v1"
  "LM Studio OpenAI-compatible API base URL."
  :type 'string)

(defcustom fork-git-lmstudio-model nil
  "LM Studio model name (as shown in LM Studio server). If nil, server default is used if any."
  :type '(choice (const :tag "Unset" nil) string))

(defcustom fork-git-openai-endpoint "https://api.openai.com/v1"
  "OpenAI-compatible API base URL."
  :type 'string)

(defcustom fork-git-openai-model "gpt-4o-mini"
  "Model name for the online OpenAI-compatible API."
  :type 'string)

(defcustom fork-git-openai-api-key-var "OPENAI_API_KEY"
  "Environment variable name holding the API key for the online backend."
  :type 'string)

(defcustom fork-git-ai-temperature 0.2
  "Sampling temperature for commit message generation."
  :type 'number)

(defcustom fork-git-commit-max-bytes 120000
  "Max bytes of diff to send to the model. Prevents very large requests."
  :type 'integer)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defun fork-git--rstrip-trailing-slash (s)
  "Remove exactly one trailing slash from string S."
  (replace-regexp-in-string "/\\'" "" (or s "")))

(defun fork-git--project-root ()
  "Find a reasonable project root (Magit toplevel > project.el > default-directory)."
  (or (when (fboundp 'magit-toplevel)
        (ignore-errors (magit-toplevel)))
      (let* ((proj (ignore-errors (and (fboundp 'project-current) (project-current))))
             (root (and proj (ignore-errors (project-root proj)))))
        root)
      default-directory))

(defun fork-git--ensure-abs-directory (path)
  "Expand PATH to an absolute directory path."
  (when path (file-name-as-directory (expand-file-name path))))

;; -----------------------------------------------------------------------------
;; Inline blame overlays (blamer) and Magit delta diffs
;; -----------------------------------------------------------------------------

;; Inline blame overlays (current line) via blamer
(use-package blamer
  :defer t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :config
  (global-blamer-mode 1))

(defun fork-git-inline-blame-toggle ()
  "Toggle inline blame overlays globally."
  (interactive)
  (require 'blamer)
  (if (bound-and-true-p global-blamer-mode)
      (global-blamer-mode -1)
    (global-blamer-mode 1)))

(defun fork-git-show-inline-commit ()
  "Show inline commit details for the current line."
  (interactive)
  (require 'blamer)
  (call-interactively 'blamer-show-commit-info))

;; Use delta for prettier diffs inside Magit (requires delta installed)
(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

(defun fork-git-magit-delta-toggle ()
  "Toggle delta-enhanced diffs for the current Magit buffer."
  (interactive)
  (require 'magit-delta)
  (if (bound-and-true-p magit-delta-mode)
      (magit-delta-mode -1)
    (magit-delta-mode 1)))

;; -----------------------------------------------------------------------------
;; Core UX: Status, blame, history, graph
;; -----------------------------------------------------------------------------

(defun fork-git-open-status ()
  "Open Magit status for the current project's root (or current directory)."
  (interactive)
  (let ((root (fork-git--project-root)))
    (magit-status-setup-buffer (or root default-directory))))

(defun fork-git-blame-toggle ()
  "Toggle Magit blame on the current buffer."
  (interactive)
  (require 'magit)
  (if (bound-and-true-p magit-blame-mode)
      (magit-blame-quit)
    (magit-blame-addition)))

(defun fork-git-file-history ()
  "Show Git history for the current file."
  (interactive)
  (require 'magit)
  (call-interactively 'magit-log-buffer-file))

(defun fork-git-branch-graph ()
  "Show a branch graph (all commits) with graph/decorate toggles."
  (interactive)
  (require 'magit)
  (let ((magit-log-arguments '("--graph" "--decorate" "--date-order")))
    (magit-log-all '())))

;; -----------------------------------------------------------------------------
;; Manual repo list: add current, open known repo
;; -----------------------------------------------------------------------------

(defun fork-git-add-current-repo ()
  "Add the current Git repository root to `fork-git-repositories` and persist."
  (interactive)
  (let* ((root (ignore-errors (magit-toplevel)))
         (dir (fork-git--ensure-abs-directory root)))
    (if (not dir)
        (message "Not inside a Git repository")
      (unless (member dir fork-git-repositories)
        (setq fork-git-repositories (append fork-git-repositories (list dir)))
        (when (fboundp 'customize-save-variable)
          (customize-save-variable 'fork-git-repositories fork-git-repositories)))
      (message "Added repo: %s" dir))))

(defun fork-git-open-known-repo ()
  "Open Magit status for a repository from `fork-git-repositories`."
  (interactive)
  (if (null fork-git-repositories)
      (message "No known repositories. Use M-x fork-git-add-current-repo to add one.")
    (let* ((choice (completing-read "Open repo: " fork-git-repositories nil t)))
      (when (and choice (stringp choice) (not (string-empty-p choice)))
        (magit-status-setup-buffer (fork-git--ensure-abs-directory choice))))))

;; -----------------------------------------------------------------------------
;; AI Commit Message: diff collection and API calls
;; -----------------------------------------------------------------------------

(defun fork-git--collect-diff ()
  "Collect staged diff if available, otherwise the working diff. Truncate if too large."
  (let* ((default-directory (or (ignore-errors (magit-toplevel)) default-directory))
         (staged (or (magit-git-string "diff" "--cached") ""))
         (diff (if (> (length staged) 0)
                   staged
                 (or (magit-git-string "diff") ""))))
    (let* ((bytes (string-bytes diff)))
      (if (> bytes fork-git-commit-max-bytes)
          (let ((trunc (substring diff 0 (min (length diff) fork-git-commit-max-bytes))))
            trunc)
        diff))) )

(defun fork-git--http-post-json (url payload &optional api-key)
  "POST PAYLOAD (JSON string) to URL and return parsed JSON object.
If API-KEY is non-nil, send it as a Bearer token. Return nil if request fails."
  (require 'url)
  (require 'json)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          (append '(("Content-Type" . "application/json"))
                  (when api-key `(("Authorization" . ,(concat "Bearer " api-key))))))
         (url-request-data (encode-coding-string payload 'utf-8))
         (buf (url-retrieve-synchronously url t t 30)))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "\n\n" nil t)
              (let ((json-object-type 'hash-table)
                    (json-array-type 'list)
                    (json-key-type 'symbol))
                (ignore-errors (json-parse-buffer :object-type 'hash-table)))))
        (kill-buffer buf)))))

(defun fork-git--join (base path)
  "Join BASE and PATH ensuring no duplicate slashes at the boundary."
  (concat (fork-git--rstrip-trailing-slash base) path))

(defun fork-git--gen-via-chat (endpoint model api-key diff)
  "Call an OpenAI-compatible chat/completions API at ENDPOINT with MODEL and DIFF.
API-KEY may be nil. Return the content string or a fallback message."
  (require 'json)
  (let* ((url (fork-git--join endpoint "/chat/completions"))
         (payload (json-encode
                   `((model . ,model)
                     (temperature . ,fork-git-ai-temperature)
                     (messages . [
                       ((role . "system")
                        (content . "You write concise, conventional, clear Git commit messages. Use imperative mood. Provide a short title and, if useful, a short bulleted body."))
                       ((role . "user")
                        (content . ,(format "Generate a high-quality Git commit message for the following git diff. Prefer Conventional Commits when appropriate (feat, fix, refactor, docs, chore). Keep width reasonable and avoid overlong lines.\n\n%s" diff)))])
                     (max_tokens . 256)))))
    (let ((resp (fork-git--http-post-json url payload api-key)))
      (or (and resp
               (let* ((choices (gethash 'choices resp))
                      (first (and choices (car choices)))
                      (msg (and first (gethash 'message first)))
                      (content (and msg (gethash 'content msg))))
                 content))
          "[AI returned no content]"))))

(defun fork-git-generate-commit-message ()
  "Generate a commit message from the current diff and insert or copy it."
  (interactive)
  (require 'magit)
  (let* ((diff (fork-git--collect-diff))
         (backend fork-git-ai-backend)
         (text (pcase backend
                 ('lmstudio (fork-git--gen-via-chat
                            (fork-git--rstrip-trailing-slash fork-git-lmstudio-endpoint)
                            (or fork-git-lmstudio-model "")
                            nil
                            diff))
                 ('openai (fork-git--gen-via-chat
                           (fork-git--rstrip-trailing-slash fork-git-openai-endpoint)
                           fork-git-openai-model
                           (getenv fork-git-openai-api-key-var)
                           diff))
                 (_ "[Unsupported backend]"))))
    (cond
     ((derived-mode-p 'git-commit-mode)
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))
     (t
      (kill-new text)
      (message "Commit message copied to clipboard")))))

;; Integration into Magit commit transient and git-commit buffer key
(with-eval-after-load 'magit
  (require 'transient)
  ;; Try to append an AI action near commit entry in the transient.
  (ignore-errors
    (transient-append-suffix 'magit-commit "c"
      '("a" "AI message" fork-git-generate-commit-message))))

;; Use Command leader for AI commit message in Magit commit buffers (see setup-keys.el)

;; -----------------------------------------------------------------------------
;; Keybindings
;; -----------------------------------------------------------------------------

;; Keybindings are provided by the unified Command leader (see setup-keys.el)
;; Expose helper commands via M-x; interactive users should use the Command leader

(provide 'setup-git)

;;; setup-git.el ends here


