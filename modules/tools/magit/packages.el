;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "2e73b66c2980abb9211d9881a8710c8ac5a33184")
  (when (featurep! +forge)
    (package! forge :pin "a44afa81f14255905d6728337abb2729b9aca840"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "60152d5c4e4b73e72e15f23ca16e8cc7734906bc")
  (package! code-review :pin "136c0933ba9dc19ce3efedb36a7dbd401e2e98b2"
    :recipe (:files ("graphql" "code-review*.el"))))
