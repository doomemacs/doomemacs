;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "2bdfc4a08f")
  (package! forge :pin "283378353a")
  (package! magit-gitflow :pin "cc41b561ec")
  (package! magit-todos :pin "a64e36574b")
  (package! github-review :pin "1de2d6d148")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "7223dca89c")))
