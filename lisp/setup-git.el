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

(use-package magit-gitflow
  :ensure t
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package pinentry
  :ensure t
  :init
  (setq epg-pinentry-mode 'loopback)
  :config
  (pinentry-start))

(require 'subr-x)  ; for string-empty-p
(require 'cl-lib)
(require 'seq)
(require 'transient)
(require 'magit-repos)
(require 'project nil t) ; for project-current/project-root (optional at startup)

(defun fork-git--set-commit-signing (symbol value)
  "Setter used by `fork-git-enable-commit-signing'."
  (set-default symbol value)
  (message "Fork Git: commit signing %s"
           (if value "enabled (--gpg-sign)" "disabled")))

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

(defcustom fork-git-recent-commit-limit 30
  "How many recent commit messages to offer for reuse."
  :type 'integer)

(defcustom fork-git-enable-commit-signing nil
  "If non-nil, pass --gpg-sign to Magit commits by default."
  :type 'boolean
  :set #'fork-git--set-commit-signing)

(defun fork-git--maybe-append-gpg-sign (args)
  "Append --gpg-sign to commit ARGS when `fork-git-enable-commit-signing' is non-nil."
  (if (and fork-git-enable-commit-signing
           (not (cl-some (lambda (arg) (string-prefix-p "--gpg-sign" arg)) args))
           (not (member "--no-gpg-sign" args)))
      (append args (list "--gpg-sign"))
    args))

(defun fork-git-toggle-commit-signing (persist)
  "Toggle default GPG signing for commits.
With PERSIST (prefix argument), save the new preference via Customize."
  (interactive "P")
  (let ((new (not fork-git-enable-commit-signing)))
    (setq fork-git-enable-commit-signing new)
    (when persist
      (when (fboundp 'customize-save-variable)
        (customize-save-variable 'fork-git-enable-commit-signing new)))
    (message "Fork Git: commit signing %s%s"
             (if new "enabled" "disabled")
             (if persist " (saved)" ""))))

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
;; Fine-grained staging, commit templates, and quick Magit wrappers
;; -----------------------------------------------------------------------------

(defmacro fork-git--define-magit-wrapper (name command docstring)
  "Define NAME as a thin wrapper over COMMAND with DOCSTRING."
  `(defun ,name ()
     ,docstring
     (interactive)
     (require 'magit)
     (call-interactively #',command)))

(defun fork-git--with-line-selection (fn)
  "Call FN after selecting the current line (or active region) inside a Magit diff."
  (unless (derived-mode-p 'magit-mode)
    (user-error "Line staging works inside Magit buffers only"))
  (if (use-region-p)
      (funcall fn)
    (let ((beg (line-beginning-position))
          (end (save-excursion
                 (forward-line 1)
                 (point))))
      (when (= beg end)
        (setq end (min (point-max) (1+ end))))
      (save-excursion
        (goto-char beg)
        (set-mark beg)
        (goto-char end)
        (activate-mark)
        (unwind-protect
            (funcall fn)
          (deactivate-mark))))))

(defun fork-git-stage-current-line ()
  "Stage only the current line (or selected region) within a Magit diff."
  (interactive)
  (fork-git--with-line-selection #'magit-stage))

(defun fork-git-unstage-current-line ()
  "Unstage only the current line (or selected region) within a Magit diff."
  (interactive)
  (fork-git--with-line-selection #'magit-unstage))

(defun fork-git-insert-recent-commit-message ()
  "Prompt for a recent commit subject and insert/copy it."
  (interactive)
  (require 'magit)
  (magit-with-toplevel
    (let* ((choices (magit-git-lines "log" "-n"
                                     (number-to-string fork-git-recent-commit-limit)
                                     "--pretty=%s"))
           (choice (cond
                    ((null choices) nil)
                    ((= (length choices) 1) (car choices))
                    (t (completing-read "Recent commit: " choices nil t)))))
      (if (or (null choice) (string-empty-p choice))
          (message "No commit history available yet.")
        (if (derived-mode-p 'git-commit-mode)
            (progn
              (goto-char (point-min))
              (insert choice "\n")
              (message "Inserted previous commit subject."))
          (kill-new choice)
          (message "Copied commit subject: %s" choice))))))

(fork-git--define-magit-wrapper fork-git-fetch-upstream
  magit-fetch-from-upstream "Fetch from the current branch upstream.")
(fork-git--define-magit-wrapper fork-git-pull-upstream
  magit-pull-from-upstream "Pull from the current branch upstream.")
(fork-git--define-magit-wrapper fork-git-push-upstream
  magit-push-current-to-upstream "Push current branch to its upstream.")
(fork-git--define-magit-wrapper fork-git-new-commit
  magit-commit "Open the Magit commit popup.")
(fork-git--define-magit-wrapper fork-git-amend-commit
  magit-commit-amend "Amend the last commit.")
(fork-git--define-magit-wrapper fork-git-create-branch
  magit-branch-and-checkout "Create and checkout a new branch.")
(fork-git--define-magit-wrapper fork-git-delete-branch
  magit-branch-delete "Delete a local branch.")
(fork-git--define-magit-wrapper fork-git-create-tag
  magit-tag-create "Create an annotated tag.")
(fork-git--define-magit-wrapper fork-git-delete-tag
  magit-tag-delete "Delete a tag.")
(fork-git--define-magit-wrapper fork-git-add-remote
  magit-remote-add "Add a new remote.")
(fork-git--define-magit-wrapper fork-git-remove-remote
  magit-remote-remove "Remove a remote.")
(fork-git--define-magit-wrapper fork-git-checkout
  magit-checkout "Checkout a branch or revision.")
(fork-git--define-magit-wrapper fork-git-cherry-pick
  magit-cherry-pick "Cherry-pick a commit.")
(fork-git--define-magit-wrapper fork-git-revert-commit
  magit-revert "Revert a commit.")
(fork-git--define-magit-wrapper fork-git-merge-branch
  magit-merge "Merge a branch.")
(fork-git--define-magit-wrapper fork-git-interactive-rebase
  magit-rebase "Start an interactive rebase.")
(fork-git--define-magit-wrapper fork-git-create-stash
  magit-stash "Create a stash.")
(fork-git--define-magit-wrapper fork-git-show-stashes
  magit-stashes "Open the stash list.")
(fork-git--define-magit-wrapper fork-git-apply-stash
  magit-stash-apply "Apply a stash without dropping it.")
(fork-git--define-magit-wrapper fork-git-pop-stash
  magit-stash-pop "Apply and drop the most recent stash.")
(fork-git--define-magit-wrapper fork-git-drop-stash
  magit-stash-drop "Drop a selected stash.")
(fork-git--define-magit-wrapper fork-git-manage-submodules
  magit-submodule "Open the submodule popup.")
(fork-git--define-magit-wrapper fork-git-open-reflog
  magit-reflog "Show the reflog for the current branch.")
(fork-git--define-magit-wrapper fork-git-open-full-log
  magit-log-all "Show the commit log for all refs.")
(fork-git--define-magit-wrapper fork-git-clone-repo
  magit-clone "Clone a repository.")
(fork-git--define-magit-wrapper fork-git-init-repo
  magit-init "Initialize a new Git repository.")
(defun fork-git-browse-commit-tree ()
  "Browse the repository tree at any revision."
  (interactive)
  (require 'magit)
  (call-interactively 'magit-find-file))

(defun fork-git-resolve-conflict ()
  "Launch Ediff-based conflict resolution for the file at point."
  (interactive)
  (require 'magit)
  (call-interactively 'magit-ediff-resolve))

(defun fork-git-open-lfs-dispatch ()
  "Open the Git LFS transient menu."
  (interactive)
  (if (require 'magit-lfs nil t)
      (call-interactively 'magit-lfs)
    (user-error "Install magit-lfs to access Git LFS helpers")))

(defun fork-git-open-gitflow-popup ()
  "Open the Git-flow popup if available."
  (interactive)
  (if (require 'magit-gitflow nil t)
      (call-interactively 'magit-gitflow-popup)
    (user-error "Install magit-gitflow to access Git-flow helpers")))

;; -----------------------------------------------------------------------------
;; Repository dashboard (folder-grouped, collapsible)
;; -----------------------------------------------------------------------------

(defvar fork-git-repo-dashboard-buffer-name "*Fork Git Repositories*")

(defvar fork-git-repo-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'fork-git-repo-dashboard-refresh)
    (define-key map (kbd "s") #'fork-git-repo-dashboard-status)
    (define-key map (kbd "RET") #'fork-git-repo-dashboard-status)
    map)
  "Keymap used inside `fork-git-repo-dashboard-mode'.")

(define-derived-mode fork-git-repo-dashboard-mode magit-section-mode "Fork-Repos"
  "Major mode for the folder-grouped Fork Git repository dashboard."
  (setq buffer-read-only t)
  (setq-local revert-buffer-function #'fork-git-repo-dashboard-refresh)
  (setq-local magit-section-initial-visibility-alist
              '((fork-git-folder . show)
                (fork-git-repo . show))))

(defun fork-git-open-repo-dashboard ()
  "Open the Fork Git repository dashboard buffer."
  (interactive)
  (unless magit-repository-directories
    (user-error "Set `magit-repository-directories' (see setup-profile.el)"))
  (let ((buf (get-buffer-create fork-git-repo-dashboard-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'fork-git-repo-dashboard-mode)
        (fork-git-repo-dashboard-mode))
      (fork-git-repo-dashboard-refresh))
    (pop-to-buffer buf)))

(defun fork-git-repo-dashboard-refresh (&rest _)
  "Regenerate the repository dashboard contents."
  (interactive)
  (let ((inhibit-read-only t)
        (groups (fork-git--collect-folder-groups)))
    (erase-buffer)
    (magit-insert-section (fork-git-dashboard)
      (magit-insert-heading
        (format "Repositories in %d folders (TAB to fold/unfold)"
                (length groups)))
      (if (null groups)
          (insert "No repositories found under `magit-repository-directories'.\n")
        (dolist (group groups)
          (fork-git--insert-repo-group (car group) (cdr group)))))
    (goto-char (point-min))
    (when-let ((section (magit-current-section)))
      (magit-section-show section))))

(defun fork-git--collect-folder-groups ()
  "Return an alist of parent folders to repository paths."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (repo (seq-uniq (magit-list-repos)))
      (let ((parent (file-name-directory (directory-file-name repo))))
        (push repo (gethash parent table))))
    (let (result)
      (maphash
       (lambda (key value)
         (push (cons key (sort value #'string-lessp)) result))
       table)
      (sort result (lambda (a b) (string-lessp (car a) (car b)))))))

(defun fork-git--insert-repo-group (folder repos)
  "Insert a magit section representing FOLDER containing REPOS."
  (magit-insert-section (fork-git-folder folder)
    (magit-insert-heading
      (format "%s (%d)"
              (abbreviate-file-name (or folder ""))
              (length repos)))
    (dolist (repo repos)
      (fork-git--insert-repo-entry repo))))

(defun fork-git--insert-repo-entry (repo)
  "Insert a single repository entry for REPO."
  (magit-insert-section (fork-git-repo repo)
    (magit-insert-heading (fork-git--format-repo-line repo))))

(defun fork-git--format-repo-line (repo)
  "Return a formatted status line string for REPO."
  (let* ((default-directory repo)
         (name (file-name-nondirectory (directory-file-name repo)))
         (branch (or (magit-get-current-branch)
                     (magit-git-string "rev-parse" "--short" "HEAD")
                     "detached"))
         (upstream (or (magit-get-upstream-branch) ""))
         (flag (or (magit-repolist-column-flag nil) " "))
         (path (abbreviate-file-name repo)))
    (format "  %s %-28s %-14s %-5s %s"
            (if (string= flag " ") " " flag)
            (propertize name 'face 'magit-section-secondary-heading)
            (propertize (format "[%s]" branch) 'face 'magit-branch-local)
            (if (string-empty-p upstream) "" upstream)
            (propertize path 'face 'shadow))))

(defun fork-git-repo-dashboard-status ()
  "Open `magit-status' for the repository at point."
  (interactive)
  (if-let ((repo (magit-section-value-if 'fork-git-repo)))
      (magit-status repo)
    (user-error "Place point on a repository entry to open it")))

;; -----------------------------------------------------------------------------
;; Fork command center (transient palette)
;; -----------------------------------------------------------------------------

(transient-define-prefix fork-git-command-center ()
  "Central Fork-like command palette on top of Magit."
  [["Status & sync"
    ("s" "Status" fork-git-open-status)
    ("f" "Fetch" fork-git-fetch-upstream)
    ("p" "Pull" fork-git-pull-upstream)
    ("u" "Push" fork-git-push-upstream)
    ("." "Magit dispatch" magit-dispatch)]
   ["Commit"
    ("c" "Commit" fork-git-new-commit)
    ("a" "Amend" fork-git-amend-commit)
    ("l" "Stage line" fork-git-stage-current-line)
    ("L" "Unstage line" fork-git-unstage-current-line)
    ("m" "AI message" fork-git-generate-commit-message)
    ("r" "Recent subject" fork-git-insert-recent-commit-message)
    ("y" "Inline commit" fork-git-show-inline-commit)]
   ["Branches & tags"
    ("n" "New branch" fork-git-create-branch)
    ("x" "Delete branch" fork-git-delete-branch)
    ("o" "Checkout" fork-git-checkout)
    ("B" "Branch graph" fork-git-branch-graph)
    ("t" "New tag" fork-git-create-tag)
    ("T" "Delete tag" fork-git-delete-tag)]
   ["Repos & remotes"
    ("+" "Add remote" fork-git-add-remote)
    ("-" "Remove remote" fork-git-remove-remote)
    ("C" "Clone repo" fork-git-clone-repo)
    ("I" "Init repo" fork-git-init-repo)
    ("A" "Remember repo" fork-git-add-current-repo)
    ("O" "Open known" fork-git-open-known-repo)
    ("D" "Repo dashboard" fork-git-open-repo-dashboard)]
   ["History & recovery"
    ("h" "File history" fork-git-file-history)
    ("v" "Toggle blame" fork-git-blame-toggle)
    ("k" "Browse tree" fork-git-browse-commit-tree)
    ("R" "Reflog" fork-git-open-reflog)
    ("H" "Full log" fork-git-open-full-log)]
   ["Stash & advanced"
    ("z" "Stash list" fork-git-show-stashes)
    ("Z" "Create stash" fork-git-create-stash)
    ("i" "Interactive rebase" fork-git-interactive-rebase)
    ("M" "Merge" fork-git-merge-branch)
    ("P" "Cherry-pick" fork-git-cherry-pick)
    ("V" "Revert" fork-git-revert-commit)
    ("E" "Resolve conflict" fork-git-resolve-conflict)
    ("S" "Submodules" fork-git-manage-submodules)
    ("'" "Toggle GPG" fork-git-toggle-commit-signing)
    ("G" "Git-flow" fork-git-open-gitflow-popup)
    (";" "Git LFS" fork-git-open-lfs-dispatch)]])

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
  (add-hook 'magit-status-sections-hook #'magit-insert-stashes t)
  (cl-pushnew '(stashes . show) magit-section-initial-visibility-alist :test #'equal)
  ;; Try to append an AI action near commit entry in the transient.
  (ignore-errors
    (transient-append-suffix 'magit-commit "c"
      '("a" "AI message" fork-git-generate-commit-message))))

(with-eval-after-load 'magit-commit
  (advice-add 'magit-commit-arguments :filter-return #'fork-git--maybe-append-gpg-sign))

;; Use Command leader for AI commit message in Magit commit buffers (see setup-keys.el)

;; -----------------------------------------------------------------------------
;; Keybindings
;; -----------------------------------------------------------------------------

;; Keybindings are provided by the unified Command leader (see setup-keys.el)
;; Expose helper commands via M-x; interactive users should use the Command leader

(provide 'setup-git)

;;; setup-git.el ends here
