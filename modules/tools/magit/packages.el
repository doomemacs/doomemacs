;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "2e0c697188ca728c240b5aa677f4a5e853692b6e")
  (when (featurep! +forge)
    (package! forge :pin "844aa6d52792dd56a14f1759312c96a3de9b988e"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "78d24cf419138b543460f40509c8c1a168b52ca0")
  (package! github-review :pin "db723740e02348c0760407e532ad667ef89210ec"))
