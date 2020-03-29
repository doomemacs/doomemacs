;;; ui/pretty-code/autoload/fira.el -*- lexical-binding: t; -*-

(defvar +pretty-code--fira-font-names
  '("FiraCode-Bold.ttf"
    "FiraCode-Light.ttf"
    "FiraCode-Medium.ttf"
    "FiraCode-Regular.ttf"
    "FiraCode-Retina.ttf"))

;;;###autoload
(defun +pretty-code/install-fira-font (&optional prefix)
  "Download and install Fira Code font based on OS.
When prefix is non-nil, ignore the prompt and just install."
  (interactive "P")
  (+pretty-code--install-font
   prefix
   "FiraCode"
   "https://github.com/tonsky/FiraCode/raw/13234c0/distr/ttf/%s"
   +pretty-code--fira-font-names))
