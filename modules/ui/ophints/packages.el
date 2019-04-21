;; -*- no-byte-compile: t; -*-
;;; ui/ophints/packages.el

(if (featurep! :editor evil)
    (package! evil-goggles)
  (package! volatile-highlights))
