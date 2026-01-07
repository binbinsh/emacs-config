;;; monokai-light-theme.el --- Custom Monokai Light port -*- lexical-binding: t; -*-

(require 'monokai-pro-theme)

(defvar monokai-light-theme-colors
  '(;; Background / foreground tuned for bright terminals (e.g., Termius Tokyo Day)
    :bg     "#f8f9fb"
    :bg+1   "#e8edf5"
    :bg+2   "#d6e3f5"
    :fg-4   "#c5cedc"
    :fg-3   "#7a8294"
    :fg-2   "#636b7a"
    :fg-1   "#2b303b"
    :fg     "#0f172a"

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
(provide-theme 'monokai-light)

(provide 'monokai-light-theme)

;;; monokai-light-theme.el ends here
