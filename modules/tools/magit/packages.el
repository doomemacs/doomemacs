;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit)
  (package! magit-gitflow)
  (if (featurep! +forge)
      (package! forge)
    (package! magithub))
  (package! magit-todos)
  (when (featurep! :feature evil +everywhere)
    (package! evil-magit)))
