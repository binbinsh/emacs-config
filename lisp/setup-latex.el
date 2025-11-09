;;; setup-latex.el --- LaTeX workspace helpers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'doc-view)

(defvar LaTeX-mode-map)

(defun my/latex--setup-highlighting ()
  "Enable LaTeX syntax highlighting using Font-Latex."
  (my/latex--font-latex-highlight))

(defun my/latex--font-latex-highlight ()
  "Font-lock highlighting powered by AUCTeX."
  (require 'font-latex)
  (font-latex-setup)
  (font-lock-mode 1)
  (font-lock-ensure))

(with-eval-after-load 'font-latex
  (setq font-latex-fontify-sectioning 1.1
        font-latex-fontify-script nil
        font-latex-match-font-metrics t))

(use-package tex
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . my/latex--setup-highlighting))
  :init
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t
        TeX-source-correlate-start-server nil
        TeX-source-correlate-method 'synctex))

(setq doc-view-continuous t
      doc-view-resolution 180
      doc-view-use-scaling t)
(dolist (binding '(("<down>" . doc-view-scroll-up-or-next-page)
                   ("<up>" . doc-view-scroll-down-or-previous-page)
                   ("<wheel-down>" . doc-view-scroll-up-or-next-page)
                   ("<wheel-up>" . doc-view-scroll-down-or-previous-page)))
  (define-key doc-view-mode-map (kbd (car binding)) (cdr binding)))
(define-key doc-view-mode-map [remap mwheel-scroll] #'my/latex--doc-view-handle-wheel)

(with-eval-after-load 'doc-view
  (setq doc-view-ghostscript-options
        '("-dSAFER" "-dNOPAUSE" "-dBATCH" "-dQUIET"
          "-sDEVICE=png16m" "-dUseTrimBox" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4"
          "-dDOINTERPOLATE" "-dBackgroundColor=16#FFFFFF" "-dAlignToPixels=0" "-dGridFitTT=0"))
  (setq doc-view-imaging-options '("-density" "180"))
  (setq doc-view-continuous t
        doc-view-use-scaling t))

(cl-defstruct my/latex-engine
  name
  binary
  args
  install-dir)

(cl-defstruct my/latex-session
  root
  main
  pdf
  engine
  explorer-window
  editor-window
  preview-window
  preview-buffer
  preview-page)

(defvar my/latex--session nil "Active LaTeX workspace state.")
(defvar my/latex--build-process nil "Current LaTeX build process, if any.")
(defvar my/latex--project-history nil)
(defvar my/latex--file-history nil)
(defvar my/latex--auto-build-hook-installed nil)

(defun my/latex--doc-view-setup ()
  "Prepare DocView buffers for the LaTeX preview pane."
  (when (bound-and-true-p pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode -1))
  (setq-local doc-view-continuous t)
  (face-remap-add-relative 'default '(:background "#ffffff"))
  (setq-local mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (setq-local mouse-wheel-progressive-speed nil)
  (my/latex--doc-view-fit-to-window))

(defun my/latex--doc-view-store-page ()
  "Remember the current DocView page so refreshes keep position."
  (when (and my/latex--session
             (eq (current-buffer) (my/latex-session-preview-buffer my/latex--session)))
    (setf (my/latex-session-preview-page my/latex--session)
          (doc-view-current-page))))

(add-hook 'doc-view-mode-hook #'my/latex--doc-view-setup)
(add-hook 'doc-view-after-change-page-hook #'my/latex--doc-view-store-page)
(add-hook 'window-size-change-functions #'my/latex--maybe-refit-preview)

(defun my/latex--doc-view-fit-to-window ()
  "Scale the preview PDF to fit the current window width."
  (when (and (eq major-mode 'doc-view-mode)
             (fboundp 'doc-view-fit-width-to-window))
    (ignore-errors
      (doc-view-fit-width-to-window))))

(defun my/latex--doc-view-handle-wheel (event)
  "Translate precision scrolling events into DocView page scrolls."
  (interactive "e")
  (pcase (and event (event-basic-type event))
    (`wheel-up (doc-view-previous-line-or-previous-page 5))
    (`wheel-down (doc-view-next-line-or-next-page 5))
    (_ (doc-view-next-line-or-next-page 5))))

(defun my/latex--pixel-scroll-guard-up (orig event)
  (if (derived-mode-p 'doc-view-mode)
      (doc-view-previous-line-or-previous-page 5)
    (funcall orig event)))

(defun my/latex--pixel-scroll-guard-down (orig event)
  (if (derived-mode-p 'doc-view-mode)
      (doc-view-next-line-or-next-page 5)
    (funcall orig event)))

(with-eval-after-load 'pixel-scroll-precision
  (advice-add 'pixel-scroll-precision-scroll-up :around #'my/latex--pixel-scroll-guard-up)
  (advice-add 'pixel-scroll-precision-scroll-down :around #'my/latex--pixel-scroll-guard-down))

(defun my/latex--maybe-refit-preview (&optional _frame)
  "Refit the preview buffer whenever the preview window is resized."
  (when (and my/latex--session
             (window-live-p (my/latex-session-preview-window my/latex--session)))
    (with-selected-window (my/latex-session-preview-window my/latex--session)
      (when (eq (window-buffer) (my/latex-session-preview-buffer my/latex--session))
        (my/latex--doc-view-fit-to-window)))))


(defun my/latex--detect-engine ()
  "Return the preferred TeX engine available on PATH."
  (let ((candidates '(("latexmk" "-pdf" "-interaction=nonstopmode" "-halt-on-error")
                      ("tectonic" "--keep-logs" "--keep-intermediates")
                      ("pdflatex" "-interaction=nonstopmode" "-halt-on-error")
                      ("xelatex" "-interaction=nonstopmode" "-halt-on-error")
                      ("lualatex" "-interaction=nonstopmode" "-halt-on-error"))))
    (cl-loop for spec in candidates
             for binary = (executable-find (car spec))
             when binary
             return (make-my/latex-engine
                     :name (car spec)
                     :binary binary
                     :args (cdr spec)
                     :install-dir (directory-file-name (file-name-directory binary))))))

(defun my/latex--detect-install-path ()
  "Locate a TeX binary and return a cons of (NAME . INSTALL-DIR)."
  (cl-loop for name in '("latexmk" "pdflatex" "xelatex" "lualatex" "latex")
           for binary = (executable-find name)
           when binary
           return (cons name (directory-file-name (file-name-directory binary)))))

(defun my/latex--default-project-root ()
  "Best guess for the LaTeX project root."
  (or (and my/latex--session (my/latex-session-root my/latex--session))
      (when-let ((proj (project-current nil)))
        (project-root proj))
      (when-let ((file buffer-file-name))
        (file-name-directory file))
      default-directory))

(defun my/latex--read-project-root ()
  "Prompt for a LaTeX project directory."
  (let* ((default (file-name-as-directory (expand-file-name (my/latex--default-project-root))))
         (chosen (read-directory-name "LaTeX project: " default nil t)))
    (setq my/latex--project-history (cons chosen (cl-remove chosen my/latex--project-history :test #'string=)))
    (file-name-as-directory (expand-file-name chosen))))

(defun my/latex--tex-files (root)
  "Return a sorted list of .tex files under ROOT."
  (seq-sort #'string-lessp
            (directory-files-recursively root "\\.tex\\'")))

(defun my/latex--select-main-file (root)
  "Pick the main TeX file under ROOT."
  (let* ((files (my/latex--tex-files root)))
    (unless files
      (user-error "No .tex files found in %s" root))
    (let* ((rel-files (mapcar (lambda (path)
                                (cons (file-relative-name path root) path))
                              files))
           (default-entry (or (seq-find (lambda (pair)
                                          (string-match-p (rx "main.tex" string-end) (car pair)))
                                        rel-files)
                                 (car rel-files))))
      (if (= (length rel-files) 1)
          (cdar rel-files)
        (let* ((choice (completing-read "Main TeX file: "
                                         (mapcar #'car rel-files)
                                         nil t nil 'my/latex--file-history
                                         (car default-entry)))
               (absolute (cdr (assoc choice rel-files))))
          (unless absolute
            (user-error "Selection not found"))
          absolute)))))

(defun my/latex--main->pdf (tex-file)
  "Return the PDF path for TEX-FILE."
  (concat (file-name-sans-extension tex-file) ".pdf"))

(defun my/latex--render-layout (session)
  "Create the three-column LaTeX workspace for SESSION."
  (delete-other-windows)
  (find-file (my/latex-session-main session))
  (let* ((editor (selected-window))
         (preview (split-window-right))
         (explorer (my/latex--ensure-explorer-window (my/latex-session-root session) editor)))
    (let* ((total (max 1 (+ (window-total-width editor) (window-total-width preview))))
           (target (max 50 (floor (* total 0.4)))))
      (window-resize preview (- target (window-total-width preview)) t))
    (with-selected-window preview
      (setf (my/latex-session-preview-buffer session)
            (my/latex--open-preview-buffer session)))
    (setf (my/latex-session-explorer-window session) explorer
          (my/latex-session-editor-window session) editor
          (my/latex-session-preview-window session) preview)
    (when (window-live-p preview)
      (with-selected-window preview
        (my/latex--doc-view-fit-to-window)))
    (select-window editor)
    session))

(defun my/latex--ensure-explorer-window (root editor-window)
  "Reuse the configured explorer sidebar for ROOT, falling back to Dired."
  (cond
   ((and (fboundp 'my/dirvish-project-root)
         (fboundp 'my/dirvish-side-window))
    (let ((default-directory root))
      (save-selected-window
        (my/dirvish-project-root)))
    (my/dirvish-side-window))
   (t
    (let ((explorer (split-window editor-window 40 'left)))
      (with-selected-window explorer
        (dired root))
      explorer))))

(defun my/latex--open-preview-buffer (session)
  "Display the PDF for SESSION if available, otherwise a placeholder buffer."
  (let ((pdf (my/latex-session-pdf session)))
    (if (file-exists-p pdf)
        (progn
          (find-file pdf)
          (my/latex--doc-view-apply-page session)
          (current-buffer))
      (let ((buffer (get-buffer-create "*LaTeX Preview*")))
        (with-current-buffer buffer
          (setq-local buffer-read-only nil)
          (erase-buffer)
          (insert "Waiting for PDF build. Save any LaTeX buffer to trigger compilation.")
          (setq-local buffer-read-only t)
          (special-mode))
        (switch-to-buffer buffer)
        buffer))))

(defun my/latex--doc-view-apply-page (session)
  "Jump DocView to the last recorded page for SESSION."
  (when (and session
             (eq major-mode 'doc-view-mode))
    (let ((target (max 1 (or (my/latex-session-preview-page session) 1))))
      (doc-view-goto-page target)
      (my/latex--doc-view-fit-to-window))))

(defun my/latex--auto-build ()
  "Rebuild when saving buffers that belong to the active session."
  (when (and my/latex--session
             buffer-file-name
             (file-in-directory-p buffer-file-name (my/latex-session-root my/latex--session)))
    (my/latex-build t)))

(defun my/latex-build (&optional silent)
  "Run the configured TeX engine for the current session."
  (interactive)
    (unless my/latex--session
      (user-error "Start a LaTeX workspace with C-c l first"))
  (unless (my/latex-session-engine my/latex--session)
    (setf (my/latex-session-engine my/latex--session)
          (or (my/latex--detect-engine)
              (user-error "No TeX engine found (install latexmk, pdflatex, xelatex, or similar)"))))
  (when (process-live-p my/latex--build-process)
    (unless silent
      (message "LaTeX build already running"))
    (cl-return-from my/latex-build nil))
  (let* ((session my/latex--session)
         (engine (my/latex-session-engine session))
         (root (my/latex-session-root session))
         (relative-main (file-relative-name (my/latex-session-main session) root))
         (default-directory root)
         (command (append (list (my/latex-engine-binary engine))
                          (my/latex-engine-args engine)
                          (list relative-main))))
    (setq my/latex--build-process
          (make-process :name "latex-build"
                        :buffer (get-buffer-create "*latex-build*")
                        :command command
                        :sentinel #'my/latex--build-sentinel))
    (unless silent
      (message "Running %s on %s" (my/latex-engine-name engine) relative-main))))

(defun my/latex--build-sentinel (proc _event)
  (when (memq (process-status proc) '(exit signal))
    (let ((success (and (eq (process-status proc) 'exit)
                        (= (process-exit-status proc) 0))))
      (setq my/latex--build-process nil)
      (if success
          (progn
            (message "LaTeX build finished")
            (my/latex--refresh-preview))
        (message "LaTeX build failed – see *latex-build* for details")))))

(defun my/latex--refresh-preview ()
  "Reload the PDF preview when it exists."
  (let* ((session my/latex--session)
         (pdf (and session (my/latex-session-pdf session))))
    (when (and pdf (file-exists-p pdf))
      (let* ((buffer (and session (my/latex-session-preview-buffer session)))
             (window (and session (my/latex-session-preview-window session))))
        (if (and buffer (buffer-live-p buffer)
                 (equal (buffer-local-value 'buffer-file-name buffer) pdf))
            (with-current-buffer buffer
              (when (eq major-mode 'doc-view-mode)
                (setf (my/latex-session-preview-page session)
                      (doc-view-current-page)))
              (revert-buffer t t)
              (my/latex--doc-view-apply-page session)
              (when (window-live-p window)
                (set-window-buffer window buffer)))
          (when (and window (window-live-p window))
            (with-selected-window window
              (find-file pdf)
              (setf (my/latex-session-preview-buffer session) (current-buffer))
              (my/latex--doc-view-apply-page session))))))))

(defun my/latex-workspace ()
  "Open the LaTeX workspace layout and remember the active session."
  (interactive)
  (let* ((root (my/latex--read-project-root))
         (main (my/latex--select-main-file root))
         (engine (or (my/latex--detect-engine)
                     (user-error "No TeX engine found. Install latexmk/pdflatex/xelatex.")))
         (pdf (my/latex--main->pdf main))
         (session (make-my/latex-session :root root :main main :pdf pdf :engine engine :preview-page 1))
         (install (my/latex--detect-install-path)))
    (setq my/latex--session (my/latex--render-layout session))
    (unless my/latex--auto-build-hook-installed
      (add-hook 'after-save-hook #'my/latex--auto-build)
      (setq my/latex--auto-build-hook-installed t))
    (when install
      (message "TeX install path %s (detected via %s)" (cdr install) (car install)))
    (my/latex-build t)))

(with-eval-after-load 'tex-mode
  (define-key LaTeX-mode-map (kbd "C-c C-b") #'my/latex-build)
  (define-key LaTeX-mode-map (kbd "C-c l") #'my/latex-workspace))

(provide 'setup-latex)
