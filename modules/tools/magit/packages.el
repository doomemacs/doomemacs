;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit)
  (package! forge)
  (package! magit-gitflow)
  (package! magit-todos)
  (package! github-review)
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit)))
