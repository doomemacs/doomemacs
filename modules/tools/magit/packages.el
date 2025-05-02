;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! magit :pin "531e7ca6190e71e6d500ddd1a1f6e5cf8402aeca") ; 4.3.3
(when (modulep! +forge)
  (package! forge :pin "9db4d386a1ce32b574e413771996d41d9b2407e8") ; 0.5.0
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "fba8fe3343665cb8000781590b2f20dc0351acb9"))
