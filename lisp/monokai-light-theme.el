;;; monokai-light-theme.el --- Custom Monokai Light port -*- lexical-binding: t; -*-

(require 'monokai-pro-theme)

(defvar monokai-light-theme-colors
  '(;; Background / foreground (ported from the Monokai Light tmTheme)
    :bg     "#ffffff"
    :bg+1   "#f5f5f5"
    :bg+2   "#ededed"
    :fg-4   "#d8d8d8"
    :fg-3   "#9f9f8f"
    :fg-2   "#7b7b73"
    :fg-1   "#404040"
    :fg     "#000000"

    ;; Accent colors
    :white  "#ffffff"
    :red    "#cd3704"
    :orange "#f25a00"
    :yellow "#ffe792"
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
