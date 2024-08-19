;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! magit :pin "cf94190410ef163fd102cdbcb67f28599d31eabc")
(package! magit-todos :pin "501c8db90ab59f8b619618b9d10db2a32a113727")
(when (modulep! +forge)
  (package! forge :pin "a56eb3cbb27c61387d35cbff6b036a2c1bc1559d")
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "e4c34fa284da25d8e0bafbae4300f1db5bdcda44"))
