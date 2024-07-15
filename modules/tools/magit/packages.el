;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "b9948f9571928bb7f39f4b3a112bd76e52a072ce")
  (when (modulep! +forge)
    (package! forge :pin "67314e2f83db358d3734183008586d8bd7cb2395")
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :pin "e4c34fa284da25d8e0bafbae4300f1db5bdcda44"))
  (package! magit-todos :pin "501c8db90ab59f8b619618b9d10db2a32a113727"))
