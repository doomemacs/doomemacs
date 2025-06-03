;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! transient :pin "25b994a565ce8035330b0a3071ee430c0282349e") ; 0.8.8
(package! magit :pin "04ee83d93fabbfbe202e9e7dc781b0dcd4d5b502") ; 4.3.5
(when (modulep! +forge)
  (package! forge :pin "9db4d386a1ce32b574e413771996d41d9b2407e8") ; 0.5.0
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "303edcfbad8190eccb9a9269dfc58ed26d386ba5"))
