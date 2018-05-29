;; -*- no-byte-compile: t; -*-
;;; emacs/ediff/packages.el

(when (featurep! :feature evil)
  (package! evil-ediff))
