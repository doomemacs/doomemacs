;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "9d1f8db507e080e032943a3ed1445bd8d9aaa9fc")
  (when (modulep! +forge)
    (package! forge :pin "0102834bb7c872c8a3f77cabf5221e8199346c43")
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :pin "e4c34fa284da25d8e0bafbae4300f1db5bdcda44"))
  (package! magit-todos :pin "501c8db90ab59f8b619618b9d10db2a32a113727"))
