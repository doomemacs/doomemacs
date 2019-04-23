;; -*- no-byte-compile: t; -*-
;;; ui/ophints/packages.el

(if (featurep! :feature evil)
    (package! evil-goggles)
  (package! volatile-highlights))
