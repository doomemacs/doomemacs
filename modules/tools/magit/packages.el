;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "c1fb53d3de6390961ccd8dfb1cc135383508d0fc")
  (package! compat :pin "cc1924fd8b3f9b75b26bf93f084ea938c06f9615")
  (when (modulep! +forge)
    (package! forge :pin "36208c43bf41782cfe81fccc904f8adbe57818e1"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "67fd80c2f10aec4d5b2a24b5d3d53c08cc1f05dc")
  (package! code-review :pin "d38fbe59304ed31c759ce733cda16f69a8ef2d8c"
    :recipe (:files ("graphql" "code-review*.el"))))
