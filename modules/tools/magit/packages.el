;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "0e8f25a8d8011328f2bf082232c720b24c2a12c2")
  (when (modulep! +forge)
    (package! forge :pin "2a3b41eb6235b3f39c017c1f86b3928a45c5a64d")
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :pin "e4c34fa284da25d8e0bafbae4300f1db5bdcda44"))
  (package! magit-todos :pin "332ce763f7336ea356964b92723678aa1ed4640f"))
