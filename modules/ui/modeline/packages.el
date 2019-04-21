;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(package! doom-modeline)
(package! anzu)
(when (featurep! :editor evil)
  (package! evil-anzu))
