;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(package! doom-modeline)
(package! anzu)
(when (featurep! :feature evil)
  (package! evil-anzu))
