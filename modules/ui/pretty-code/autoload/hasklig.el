;;; ui/pretty-code/autoload/hasklig.el -*- lexical-binding: t; -*-

(defvar +pretty-code--hasklig-font-names
  '("Hasklig-Black.otf"
    "Hasklig-BlackIt.otf"
    "Hasklig-Bold.otf"
    "Hasklig-BoldIt.otf"
    "Hasklig-ExtraLight.otf"
    "Hasklig-ExtraLightIt.otf"
    "Hasklig-It.otf"
    "Hasklig-Light.otf"
    "Hasklig-LightIt.otf"
    "Hasklig-Medium.otf"
    "Hasklig-MediumIt.otf"
    "Hasklig-Regular.otf"
    "Hasklig-Semibold.otf"
    "Hasklig-SemiboldIt.otf"))

;;;###autoload
(defun +pretty-code/install-hasklig-font (&optional prefix)
  "Download and install Hasklig font based on OS.
When prefix is non-nil, ignore the prompt and just install."
  (interactive "P")
  (+pretty-code--install-font
   prefix
   "Hasklig"
   "https://github.com/jsravn/hasklig-emacs/raw/33354a3/%s"
   +pretty-code--hasklig-font-names))
