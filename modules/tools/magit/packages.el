;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "65ecb9c5fc7586a1c527b60d180a97ea230da99f")
  (when (modulep! +forge)
    (package! forge :pin "03b48be2a12a282cd47b92287fc1701a81f1cece")
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :pin "2670a4beb6636e6ee596c5b7cb5e784cf33d5a98"))
  (package! magit-todos :pin "1e9acc0ba63fbc297001bf334d63cb4326be80df"))
