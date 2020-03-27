;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "68b5a13fa1")
  (package! forge :pin "2e2d26cf42")
  (package! magit-gitflow :pin "cc41b561ec")
  (package! magit-todos :pin "a0e5d1f3c7")
  (package! github-review :pin "50c6bcc7cf")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "0b79aa33a4")))
