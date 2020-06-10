;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "793e387a508984ae2586b93ff78afddf7a1164ac")
  (when (featurep! +forge)
    (package! forge :pin "6f299d2d84a0c92a6656a6db03656c2d554d2cac"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "a0e5d1f3c7dfcb4f18c1b0d57f1746a4872df5c6")
  (package! github-review :pin "50c6bcc7cf4d7193577b3f74eea4dd72f2b7795b")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "88dc26ce59dbf4acb4e2891c79c4bd329553ba56")))
