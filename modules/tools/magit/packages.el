;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "54d37dc14c3f715dd0328a70bc65d63c54ee9613")
  (when (modulep! +forge)
    (package! forge :pin "b16b6ec4f7612f5a8fc6d50133cc6189f062c183")
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :pin "2670a4beb6636e6ee596c5b7cb5e784cf33d5a98"))
  (package! magit-todos :pin "debb77b3589f2d83c8b43706edc1f8f90bf1ad91"))
