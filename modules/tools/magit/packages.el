;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "877c389ca0161959081fa2c77045ce1ae9463be4")
  (when (featurep! +forge)
    (package! forge :pin "41efa674cff0b447efbc103494fd61ec9b9156ae"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "60152d5c4e4b73e72e15f23ca16e8cc7734906bc")
  (package! code-review :pin "f1a79c20ae51d23f76067a1e5a2f5c1c4db42ec9"
    :recipe (:files ("graphql" "code-review*.el"))))
