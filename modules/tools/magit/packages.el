;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "65c4485e19bf570ebcb81fbaa6352c4e94bb05da")
  (when (featurep! +forge)
    (package! forge :pin "402773ef7e83ddfab64bfee23daea2776d50dbc1"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "60152d5c4e4b73e72e15f23ca16e8cc7734906bc")
  (package! code-review :pin "b0bedbdb30e019ed8c40fedf1087c3ad28e72c59"
    :recipe (:files ("graphql" "code-review*.el"))))
