;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

;; NOTE: Always bump this to HEAD~1, not HEAD, because the latest commit on
;;   magit's melpa branch is auto-generated and moved to HEAD every time there's
;;   a commit to its main branch.
(package! magit :pin "2da34f1317c619ec2dfb9e0d969449261ca7f31f")
(when (modulep! +forge)
  (package! forge :pin "1e7ee99c7f76034e40210a6fd6007015b1998f6d")
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "e4c34fa284da25d8e0bafbae4300f1db5bdcda44"))
