(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)
(setq ls-lisp-dirs-first t)

(require 'dired)

;; Packages for modern Dired UX
(use-package dirvish
  :config
  (dirvish-override-dired-mode)
  ;; Attributes shown in header/columns
  (setq dirvish-attributes '(all-the-icons file-size file-time git-msg)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (when (display-graphic-p) (all-the-icons-dired-mode 1)))))

(use-package dired-git-info :after dired)
(use-package peep-dired :after dired)

;; Dired usability
(setq dired-dwim-target t)                     ;; default to other pane for copy/move
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always)
(setq dired-listing-switches "-alh")          ;; human-readable sizes
(setq global-auto-revert-non-file-buffers t)   ;; auto refresh dired buffers
(add-hook 'dired-mode-hook #'auto-revert-mode)

;; Visuals in Dired buffers
(add-hook 'dired-mode-hook (lambda ()
  (when (fboundp 'dired-hide-details-mode) (dired-hide-details-mode 1))
  (hl-line-mode 1)))

;; Colorful Dired faces are enabled via use-package diredfl above

;; Icons in Dired are enabled via use-package all-the-icons-dired above

;; Git info inline toggle key
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd ")") #'dired-git-info-mode))

;; Dirvish configured via use-package above

;; Quick preview toggle (Dirvish peek or peep-dired)
(defun tc/dired-toggle-preview ()
  "Toggle quick preview in Dirvish (peek) or Dired (peep-dired)."
  (interactive)
  (cond
    ((and (boundp 'dirvish-mode) dirvish-mode (fboundp 'dirvish-peek-mode))
     (if (bound-and-true-p dirvish-peek-mode)
         (dirvish-peek-mode -1)
       (dirvish-peek-mode 1)))
    ((derived-mode-p 'dired-mode)
     (when (require 'peep-dired nil t)
       (if (bound-and-true-p peep-dired-mode)
           (peep-dired -1)
         (peep-dired 1))))))

;; Two-pane helper (like Total Commander)
(defun tc/dired-two-panes ()
  "Open a two-pane Dirvish/Dired session in current directory."
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

;; Keybindings: single keys in Dired/Dirvish buffers
(with-eval-after-load 'dired
  (let ((map dired-mode-map))
    ;; Preview
    (define-key map (kbd "V") #'tc/dired-toggle-preview)
    ;; Open/Edit
    (define-key map (kbd "RET") #'dired-find-file)
    ;; Copy/Move/NewDir/Delete
    (define-key map (kbd "C") #'dired-do-copy)
    (define-key map (kbd "R") #'dired-do-rename)
    (define-key map (kbd "+") #'dired-create-directory)
    (define-key map (kbd "D") #'dired-do-delete)
    ;; Pane switch on TAB
    (define-key map (kbd "TAB") #'other-window)))

(with-eval-after-load 'dirvish
  (let ((map dirvish-mode-map))
    (define-key map (kbd "V") #'tc/dired-toggle-preview)
    (define-key map (kbd "RET") #'dired-find-file)
    (define-key map (kbd "C") #'dired-do-copy)
    (define-key map (kbd "R") #'dired-do-rename)
    (define-key map (kbd "+") #'dired-create-directory)
    (define-key map (kbd "D") #'dired-do-delete)
    (define-key map (kbd "TAB") #'other-window)
    ))

(provide 'setup-dired)
