;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit)
  (package! magit-gitflow)
  (package! magithub)
  (package! magit-todos)
  (package! with-editor)
  (when (featurep! :feature evil +everywhere)
    (package! evil-magit)))
