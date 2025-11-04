;;; monokai-pro-light-theme.el --- Custom Monokai Pro light variant -*- lexical-binding: t; -*-

(require 'monokai-pro-theme)

(defvar monokai-pro-light-theme-colors
  '(;; Background / foreground (based on Monokai Light .tmTheme)
    :bg     "#fafafa"
    :bg+1   "#e6e3c3"
    :bg+2   "#ccc9ad"
    :fg-4   "#b7b4a0"
    :fg-3   "#999680"
    :fg-2   "#75715e"
    :fg-1   "#666663"
    :fg     "#49483e"

    ;; Accent colors
    :white  "#ffffff"
    :red    "#f9005a"
    :orange "#cf7000"
    :yellow "#998f2f"
    :green  "#679c00"
    :blue   "#0089b3"
    :purple "#684d99"
    :pink   "#f9005a"

    ;; Legacy accents (keep to reuse original definitions)
    :orig-red     "#f92672"
    :orig-orange  "#fd971f"
    :orig-yellow  "#e6db74"
    :orig-green   "#a6e22e"
    :orig-cyan    "#a1efe4"
    :orig-blue    "#66d9ef"
    :orig-violet  "#ae81ff"
    :orig-magenta "#fd5ff0"))

(deftheme monokai-pro-light)
(monokai-pro-theme-define 'monokai-pro-light monokai-pro-light-theme-colors)
(provide-theme 'monokai-pro-light)

(provide 'monokai-pro-light-theme)

;;; monokai-pro-light-theme.el ends here
