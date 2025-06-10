;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(package! transient :pin "f3f498aa155f88c7e2ab6d1d01d1361813059db8") ; 0.9.2
(package! magit :pin "2f1ff91f128f28aa277e0e060ef44b4be8a989c1") ; 4.3.6
(when (modulep! +forge)
  (package! forge :pin "a31859547a1ea5e2acbab67b6b64f90134e2a156") ; 0.5.3
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "303edcfbad8190eccb9a9269dfc58ed26d386ba5"))
