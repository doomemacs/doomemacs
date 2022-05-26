;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "a4a78d341a7006ccdec708b424048ba3b22ee801")
  (when (featurep! +forge)
    (package! forge :pin "66b3993c98c724e0d3e6411036ccdfab95c7a504"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "67fd80c2f10aec4d5b2a24b5d3d53c08cc1f05dc")
  (package! code-review :pin "d38fbe59304ed31c759ce733cda16f69a8ef2d8c"
    :recipe (:files ("graphql" "code-review*.el"))))
