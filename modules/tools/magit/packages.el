;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "c8cd22e28d")
  (package! forge :pin "0ade907a46")
  (package! magit-gitflow :pin "cc41b561ec")
  (package! magit-todos :pin "ad5663aa26")
  (package! github-review :pin "1de2d6d148")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "0b79aa33a4")))
