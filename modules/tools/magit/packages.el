;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "b5637d665c1e5bd5b76ffb072dbac387f37a5f63")
  (when (modulep! +forge)
    (package! forge :pin "ad94b5665de357347bfc52910eef46a79f74988d")
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :pin "e4c34fa284da25d8e0bafbae4300f1db5bdcda44"))
  (package! magit-todos :pin "332ce763f7336ea356964b92723678aa1ed4640f"))
