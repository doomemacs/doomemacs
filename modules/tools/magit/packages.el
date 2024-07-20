;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "9d4192b7b12c6b7f0664d99c4f876cfcc0a30ad4")
  (when (modulep! +forge)
    (package! forge :pin "9edfcb2c1528dc7e607daa2d8fa655fc80e0d8b7")
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :pin "e4c34fa284da25d8e0bafbae4300f1db5bdcda44"))
  (package! magit-todos :pin "501c8db90ab59f8b619618b9d10db2a32a113727"))
