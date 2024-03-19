;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "0963697f24cfbe80f92312044bd9ab28b914b053")
  (when (modulep! +forge)
    (package! forge :pin "68771ca4d53c3aea5c860eeb888cee8e9cb5ca37")
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :pin "e4c34fa284da25d8e0bafbae4300f1db5bdcda44"))
  (package! magit-todos :pin "1e9acc0ba63fbc297001bf334d63cb4326be80df"))
