;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "aba0a596115b42fbd60347d893bcc319020ce5a2")
  (when (featurep! +forge)
    (package! forge :pin "72b29bd7bc4172705b55bdd2a5070202ec154069"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "60152d5c4e4b73e72e15f23ca16e8cc7734906bc")
  (package! github-review :pin "2a24e75dfc2d9f37789ff60b4c10deb7c96f3f88"))
