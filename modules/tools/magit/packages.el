;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit)
  (package! forge)
  (package! magit-gitflow)
  (package! magit-todos)
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit)))
