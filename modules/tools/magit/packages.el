;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! magit :pin "3b37048688e50ba5897945ce08eeaed34433c9b7")
(when (modulep! +forge)
  (package! forge :pin "1e7ee99c7f76034e40210a6fd6007015b1998f6d")
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "e4c34fa284da25d8e0bafbae4300f1db5bdcda44"))
