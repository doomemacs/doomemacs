;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "21454777281247d97814ce5fb64f4afe39fab5da")
  (when (featurep! +forge)
    (package! forge :pin "e340c2be2aa5337c8c4c81cd6eab87961c6848b6"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "78d24cf419138b543460f40509c8c1a168b52ca0")
  (package! github-review :pin "db723740e02348c0760407e532ad667ef89210ec"))
