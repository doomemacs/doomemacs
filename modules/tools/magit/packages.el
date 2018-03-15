;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! magit)
(package! magithub)
(when (featurep! :feature evil)
  (package! evil-magit))
