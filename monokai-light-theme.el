;;; monokai-light-theme.el --- Custom Monokai Light port -*- lexical-binding: t; -*-

(require 'monokai-pro-theme)

;;; User customization for terminal background detection
(defvar monokai-light-terminal-bg 'auto
  "Terminal background type. Can be 'light, 'dark, or 'auto.
When 'auto, tries to detect from COLORFGBG environment variable.")

;;; Light terminal colors (tokyonight day theme)
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

;;; Dark terminal colors (tokyonight night/storm theme)
(defvar monokai-light-dark-terminal-colors
  '(:fg         "#c0caf5"    ;; tokyonight foreground
    :fg-1       "#a9b1d6"    ;; tokyonight foreground alt
    :fg-2       "#7aa2f7"    ;; tokyonight blue
    :fg-3       "#565f89"    ;; tokyonight comment
    :fg-4       "#414868"    ;; tokyonight dark3
    :cyan       "#7dcfff"    ;; tokyonight cyan
    :blue       "#7aa2f7"    ;; tokyonight blue
    :purple     "#bb9af7"    ;; tokyonight magenta
    :orange     "#ff9e64"    ;; tokyonight orange
    :red        "#f7768e"    ;; tokyonight red
    :yellow     "#e0af68"    ;; tokyonight yellow
    :green      "#9ece6a"    ;; tokyonight green
    :green-dark "#73daca"    ;; tokyonight teal
    :magenta    "#ff007c")   ;; tokyonight magenta2
  "Colors optimized for dark terminal backgrounds like tokyonight night.")

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
   `(diredfl-read-priv ((t (:foreground "#1a6600"))))  ;; 深绿色，更易读
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
   `(nerd-icons-dired-dir-face ((t (:foreground ,cyan))))

   ;; Vterm colors (light theme friendly)
   `(vterm-color-black ((t (:foreground ,fg-1 :background ,fg-3))))
   `(vterm-color-red ((t (:foreground ,red :background ,red))))
   `(vterm-color-green ((t (:foreground "#629755" :background "#629755"))))
   `(vterm-color-yellow ((t (:foreground ,yellow :background ,yellow))))
   `(vterm-color-blue ((t (:foreground ,fg-2 :background ,fg-2))))
   `(vterm-color-magenta ((t (:foreground "#9c27b0" :background "#9c27b0"))))
   `(vterm-color-cyan ((t (:foreground ,cyan :background ,cyan))))
   `(vterm-color-white ((t (:foreground ,fg-3 :background "#f8f8f2"))))))

;; Terminal background detection
(defun monokai-light--detect-terminal-bg ()
  "Detect terminal background type (light or dark).
Returns 'light or 'dark based on COLORFGBG env var or user setting."
  (if (eq monokai-light-terminal-bg 'auto)
      (let ((colorfgbg (getenv "COLORFGBG")))
        (cond
         ;; COLORFGBG format: "fg;bg" - bg >= 8 usually means dark terminal
         ((and colorfgbg (string-match ";\\([0-9]+\\)" colorfgbg))
          (let ((bg-color (string-to-number (match-string 1 colorfgbg))))
            (if (< bg-color 8) 'dark 'light)))
         ;; Check for common dark terminal indicators
         ((string-match-p "dark\\|night\\|storm" (or (getenv "TERM_PROGRAM") ""))
          'dark)
         ;; Default to dark for modern terminals
         (t 'dark)))
    monokai-light-terminal-bg))

;; Terminal mode: use terminal's background color instead of theme's
;; This allows seamless integration with terminal themes like tokyonight
(defun monokai-light-terminal-setup ()
  "Adjust faces for terminal mode to use terminal's background."
  (unless (display-graphic-p)
    (let* ((is-dark (eq (monokai-light--detect-terminal-bg) 'dark))
           (colors (if is-dark monokai-light-dark-terminal-colors monokai-light-theme-colors))
           (fg     (plist-get colors :fg))
           (fg-1   (plist-get colors :fg-1))
           (fg-2   (plist-get colors :fg-2))
           (fg-3   (plist-get colors :fg-3))
           (cyan   (plist-get colors :cyan))
           (blue   (plist-get colors :blue))
           (purple (plist-get colors :purple))
           (orange (plist-get colors :orange))
           (red    (plist-get colors :red))
           (yellow (plist-get colors :yellow))
           (green  (plist-get colors :green)))

      ;; Use terminal's background
      (set-face-background 'default "unspecified-bg")
      (set-face-background 'fringe "unspecified-bg")
      (set-face-background 'line-number "unspecified-bg")
      (when (facep 'header-line)
        (set-face-background 'header-line "unspecified-bg"))

      ;; hl-line: use visible background color
      (if is-dark
          (progn
            ;; Dark terminal: use darker highlight to avoid clashing with text colors
            (set-face-attribute 'hl-line nil
                                :background "#232433"
                                :underline nil
                                :extend t)
            (when (facep 'dirvish-hl-line)
              (set-face-attribute 'dirvish-hl-line nil
                                  :background "#232433"
                                  :underline nil
                                  :extend t)))
        ;; Light terminal
        (set-face-attribute 'hl-line nil
                            :background "#cfd0d5"
                            :underline nil
                            :extend t)
        (when (facep 'dirvish-hl-line)
          (set-face-attribute 'dirvish-hl-line nil
                              :background "#cfd0d5"
                              :underline nil
                              :extend t)))

      ;; Apply foreground colors based on terminal background
      (when is-dark
        ;; Core faces
        (set-face-foreground 'default fg)
        (set-face-foreground 'font-lock-comment-face fg-3)
        (set-face-foreground 'font-lock-string-face green)
        (set-face-foreground 'font-lock-keyword-face purple)
        (set-face-foreground 'font-lock-function-name-face blue)
        (set-face-foreground 'font-lock-variable-name-face fg)
        (set-face-foreground 'font-lock-type-face cyan)
        (set-face-foreground 'font-lock-constant-face orange)
        (set-face-foreground 'font-lock-builtin-face red)
        (set-face-foreground 'font-lock-warning-face yellow)

        ;; Line numbers
        (set-face-foreground 'line-number fg-3)
        (set-face-foreground 'line-number-current-line fg)

        ;; Dired faces
        (when (facep 'dired-directory)
          (set-face-foreground 'dired-directory cyan))
        (when (facep 'dired-symlink)
          (set-face-foreground 'dired-symlink purple))

        ;; Diredfl faces
        (when (facep 'diredfl-dir-name)
          (set-face-foreground 'diredfl-dir-name cyan))
        (when (facep 'diredfl-file-name)
          (set-face-foreground 'diredfl-file-name fg))
        (when (facep 'diredfl-file-suffix)
          (set-face-foreground 'diredfl-file-suffix fg-2))
        (when (facep 'diredfl-symlink)
          (set-face-foreground 'diredfl-symlink purple))
        (when (facep 'diredfl-dir-priv)
          (set-face-foreground 'diredfl-dir-priv cyan))
        (when (facep 'diredfl-read-priv)
          (set-face-foreground 'diredfl-read-priv green))
        (when (facep 'diredfl-write-priv)
          (set-face-foreground 'diredfl-write-priv orange))
        (when (facep 'diredfl-exec-priv)
          (set-face-foreground 'diredfl-exec-priv red))
        (when (facep 'diredfl-number)
          (set-face-foreground 'diredfl-number fg-2))
        (when (facep 'diredfl-date-time)
          (set-face-foreground 'diredfl-date-time fg-3))
        (when (facep 'diredfl-no-priv)
          (set-face-foreground 'diredfl-no-priv fg-3))

        ;; Minibuffer / completion
        (when (facep 'minibuffer-prompt)
          (set-face-foreground 'minibuffer-prompt blue))

        ;; Mode line - need both foreground and background for visibility
        (when (facep 'mode-line)
          (set-face-attribute 'mode-line nil
                              :foreground "#c0caf5"
                              :background "#1a1b26"))
        (when (facep 'mode-line-inactive)
          (set-face-attribute 'mode-line-inactive nil
                              :foreground "#565f89"
                              :background "#16161e"))))))

(add-hook 'after-init-hook #'monokai-light-terminal-setup)
(add-hook 'after-make-frame-functions
          (lambda (_frame) (monokai-light-terminal-setup)))

(provide-theme 'monokai-light)

(provide 'monokai-light-theme)

;;; monokai-light-theme.el ends here
