;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "62dfe5a9dde309b562871ef93fde3d8fb37a5870")
  (when (featurep! +forge)
    (package! forge :pin "8683b148d3ce1413aeb4b6dde1b6f55610b5aaf5"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "78d24cf419138b543460f40509c8c1a168b52ca0")
  (package! github-review :pin "d0c8234cf523818513f892f30153210606abb6be"))
