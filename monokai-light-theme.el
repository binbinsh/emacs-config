;;; monokai-light-theme.el --- Custom Monokai Light port -*- lexical-binding: t; -*-

(require 'monokai-pro-theme)

(defvar monokai-light-theme-colors
  '(;; Background / foreground tuned for tokyonight day theme
    :bg     "#e1e2e7"      ;; tokyonight day background
    :bg+1   "#d5d6db"      ;; slightly darker for highlights
    :bg+2   "#c8c9ce"      ;; even darker for secondary highlights
    :fg-4   "#a1a6c5"      ;; tokyonight day comment color
    :fg-3   "#6172b0"      ;; tokyonight day dark foreground
    :fg-2   "#3760bf"      ;; tokyonight day blue
    :fg-1   "#3b4261"      ;; tokyonight day foreground alt
    :fg     "#3760bf"      ;; tokyonight day foreground

    ;; Accent colors
    :white  "#ffffff"
    :red    "#cd3704"
    :orange "#f25a00"
    :yellow "#b15c00"  ;; darken yellow so strings/read highlights pop on light bg
    :green  "#6aaf19"
    :blue   "#28c6e4"
    :cyan   "#0089b3"
    :purple "#ae81ff"
    :pink   "#f92672"

    ;; Legacy accents (keep around for packages which reference the original palette)
    :orig-red     "#f92672"
    :orig-orange  "#fd971f"
    :orig-yellow  "#e6db74"
    :orig-green   "#a6e22e"
    :orig-cyan    "#a1efe4"
    :orig-blue    "#66d9ef"
    :orig-violet  "#ae81ff"
    :orig-magenta "#fd5ff0"))

(deftheme monokai-light)
(monokai-pro-theme-define 'monokai-light monokai-light-theme-colors)

;; Additional face customizations for light theme
(let ((cyan   (plist-get monokai-light-theme-colors :cyan))
      (blue   (plist-get monokai-light-theme-colors :blue))
      (orange (plist-get monokai-light-theme-colors :orange))
      (fg     (plist-get monokai-light-theme-colors :fg))
      (fg-1   (plist-get monokai-light-theme-colors :fg-1))
      (fg-2   (plist-get monokai-light-theme-colors :fg-2))
      (fg-3   (plist-get monokai-light-theme-colors :fg-3))
      (bg+1   (plist-get monokai-light-theme-colors :bg+1))
      (red    (plist-get monokai-light-theme-colors :red))
      (purple (plist-get monokai-light-theme-colors :purple))
      (yellow (plist-get monokai-light-theme-colors :yellow)))

  (custom-theme-set-faces
   'monokai-light

   ;; Dired - directory colors (use cyan for better visibility on light bg)
   `(dired-directory ((t (:foreground ,cyan :weight semi-bold))))
   `(dired-symlink ((t (:foreground ,purple))))
   `(dired-perm-write ((t (:foreground ,orange))))
   `(dired-marked ((t (:foreground ,red :weight bold))))
   `(dired-flagged ((t (:foreground ,red :weight bold))))
   `(dired-header ((t (:foreground ,fg :weight bold))))

   ;; Diredfl - enhanced dired colors
   `(diredfl-dir-name ((t (:foreground ,cyan :weight semi-bold))))
   `(diredfl-dir-heading ((t (:foreground ,fg :weight bold :background ,bg+1))))
   `(diredfl-file-name ((t (:foreground ,fg))))
   `(diredfl-file-suffix ((t (:foreground ,fg-2))))
   `(diredfl-symlink ((t (:foreground ,purple))))
   `(diredfl-link-priv ((t (:foreground ,purple))))
   `(diredfl-no-priv ((t (:foreground ,fg-3))))
   `(diredfl-dir-priv ((t (:foreground ,cyan))))
   `(diredfl-read-priv ((t (:foreground ,yellow))))
   `(diredfl-write-priv ((t (:foreground ,orange))))
   `(diredfl-exec-priv ((t (:foreground ,red))))
   `(diredfl-rare-priv ((t (:foreground ,purple))))
   `(diredfl-number ((t (:foreground ,fg-2))))
   `(diredfl-date-time ((t (:foreground ,fg-3))))
   `(diredfl-deletion ((t (:foreground ,red :weight bold))))
   `(diredfl-deletion-file-name ((t (:foreground ,red))))
   `(diredfl-flag-mark ((t (:foreground ,orange :weight bold))))
   `(diredfl-flag-mark-line ((t (:background ,bg+1))))
   `(diredfl-ignored-file-name ((t (:foreground ,fg-3))))
   `(diredfl-compressed-file-name ((t (:foreground ,blue))))
   `(diredfl-compressed-file-suffix ((t (:foreground ,blue))))
   `(diredfl-executable-tag ((t (:foreground ,red))))

   ;; Dirvish
   `(dirvish-hl-line ((t (:background ,bg+1))))

   ;; Nerd-icons colors (for dired/dirvish icons)
   `(nerd-icons-dired-dir-face ((t (:foreground ,cyan))))))

;; Terminal mode: use terminal's background color instead of theme's
;; This allows seamless integration with terminal themes like tokyonight day
(defun monokai-light-terminal-setup ()
  "Adjust faces for terminal mode to use terminal's background."
  (unless (display-graphic-p)
    (set-face-background 'default "unspecified-bg")
    (set-face-background 'fringe "unspecified-bg")
    (set-face-background 'line-number "unspecified-bg")
    (set-face-background 'hl-line nil)
    (when (facep 'dirvish-hl-line)
      (set-face-background 'dirvish-hl-line nil))
    (when (facep 'header-line)
      (set-face-background 'header-line "unspecified-bg"))))

(add-hook 'after-init-hook #'monokai-light-terminal-setup)
(add-hook 'after-make-frame-functions
          (lambda (_frame) (monokai-light-terminal-setup)))

(provide-theme 'monokai-light)

(provide 'monokai-light-theme)

;;; monokai-light-theme.el ends here
