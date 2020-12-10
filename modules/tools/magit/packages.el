;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "acfe22ab60a56c61aae3ca6d4f2b7b826fe3b071")
  (when (featurep! +forge)
    (package! forge :pin "953764d2bb57b6bbaec4a2048722050fd15732db"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "78d24cf419138b543460f40509c8c1a168b52ca0")
  (package! github-review :pin "db723740e02348c0760407e532ad667ef89210ec"))
