;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit)
  (package! magithub)
  (package! magit-gitflow)
  (when (featurep! :feature evil)
    (package! evil-magit)))
