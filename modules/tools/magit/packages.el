;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "0ac05f39624d1d8f2cd6cb2d2f1aa2e387a9c70a")
  (when (featurep! +forge)
    (package! forge :pin "0ff9b8a0dea2483203646ba82ce155bb4957a88a"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "60152d5c4e4b73e72e15f23ca16e8cc7734906bc")
  (package! code-review :pin "ccc3795a72554439f230969322c0e3239252c193"
    :recipe (:files ("graphql" "code-review*.el"))))
