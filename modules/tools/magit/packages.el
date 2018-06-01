;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit)
  (package! magit-gitflow)
  (package! magithub)
  (when (featurep! :feature evil +everywhere)
    (package! evil-magit)))
