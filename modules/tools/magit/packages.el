;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "e77782272082977d56635308b89cf686abd87ef5")
  (when (modulep! +forge)
    (package! forge :pin "21d410c8102422f32db14ddac4eae9e28c2b5c5a")
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :pin "e4c34fa284da25d8e0bafbae4300f1db5bdcda44"))
  (package! magit-todos :pin "501c8db90ab59f8b619618b9d10db2a32a113727"))
