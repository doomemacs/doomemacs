;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "877c389ca0161959081fa2c77045ce1ae9463be4")
  (when (featurep! +forge)
    (package! forge :pin "41efa674cff0b447efbc103494fd61ec9b9156ae"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "60152d5c4e4b73e72e15f23ca16e8cc7734906bc")
  (package! github-review :pin "725fbc7b385228f53a7ddc46a92c1276bab4aea8"))
