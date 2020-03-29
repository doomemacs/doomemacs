;;; ui/pretty-code/autoload/iosevka.el -*- lexical-binding: t; -*-

(defvar +pretty-code--iosevka-font-names
  '("iosevka-custom-lightoblique.ttf"
    "iosevka-custom-thinoblique.ttf"
    "iosevka-custom-mediumitalic.ttf"
    "iosevka-custom-light.ttf"
    "iosevka-custom-heavy.ttf"
    "iosevka-custom-bolditalic.ttf"
    "iosevka-custom-bold.ttf"
    "iosevka-custom-lightitalic.ttf"
    "iosevka-custom-thin.ttf"
    "iosevka-custom-extralight.ttf"
    "iosevka-custom-oblique.ttf"
    "iosevka-custom-italic.ttf"
    "iosevka-custom-heavyoblique.ttf"
    "iosevka-custom-heavyitalic.ttf"
    "iosevka-custom-extralightitalic.ttf"
    "iosevka-custom-thinitalic.ttf"
    "iosevka-custom-medium.ttf"
    "iosevka-custom-mediumoblique.ttf"
    "iosevka-custom-extralightoblique.ttf"
    "iosevka-custom-boldoblique.ttf"
    "iosevka-custom-regular.ttf"))

;;;###autoload
(defun +pretty-code/install-iosevka-font (&optional prefix)
  "Download and install Iosevka font based on OS.
When prefix is non-nil, ignore the prompt and just install."
  (interactive "P")
  (+pretty-code--install-font
   prefix
   "Iosevka"
   "https://github.com/jsravn/iosevka-emacs/raw/20fc2c4/%s"
   +pretty-code--iosevka-font-names))
