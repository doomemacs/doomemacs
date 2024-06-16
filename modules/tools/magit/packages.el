;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "f9268a959828d0c6ab26171dd2fb1ffc55e5ae70")
  (when (modulep! +forge)
    (package! forge :pin "c3675fd068767c694177a310d4fa0a01f81bb2d3")
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :pin "e4c34fa284da25d8e0bafbae4300f1db5bdcda44"))
  (package! magit-todos :pin "501c8db90ab59f8b619618b9d10db2a32a113727"))
