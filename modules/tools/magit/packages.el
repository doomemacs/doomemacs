;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit)
  (package! magit-gitflow)
  (when (featurep! +hub)
    (package! magithub))
  (when (featurep! :feature evil +everywhere)
    (package! evil-magit)))
