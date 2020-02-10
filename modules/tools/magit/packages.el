;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "57f2d0f830")
  (package! forge :pin "0081afd2c8")
  (package! magit-gitflow :pin "cc41b561ec")
  (package! magit-todos :pin "ad5663aa26")
  (package! github-review :pin "1de2d6d148")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "1bfd546be8")))
