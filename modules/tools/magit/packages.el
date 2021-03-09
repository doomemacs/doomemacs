;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "2277752a176f0b649fd72424d3427bf28184802e")
  (when (featurep! +forge)
    (package! forge :pin "fa80a8789cb15c1b1c5fc7a9e328283202c75135"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "78d24cf419138b543460f40509c8c1a168b52ca0")
  (package! github-review :pin "d0c8234cf523818513f892f30153210606abb6be"))
