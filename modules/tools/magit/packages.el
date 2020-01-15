;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "5503aa1857e78bb43502261956173a66c9b88c71")
  (package! forge :pin "269effb4954071f590954c0e5b4faba3f27d03d0")
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "a64e36574bcb77a86726922df905307e55ea62ed")
  (package! github-review :pin "e8a275939e1a774c84b71ab3df2ce1599445dab0")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "7223dca89c0b4bca9871c453a30a4d4edfdb444e")))
