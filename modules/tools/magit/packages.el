;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "8cb6cdf3e4")
  (package! forge :pin "fb04716b64")
  (package! magit-gitflow :pin "cc41b561ec")
  (package! magit-todos :pin "ad5663aa26")
  (package! github-review :pin "1de2d6d148")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "1bfd546be8")))
