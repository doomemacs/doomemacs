;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "4735b9254105eb7dd538f979d8b4c6ab96b827b9")
  (when (featurep! +forge)
    (package! forge :pin "8382fd3d43855de779c46da338dd59b1cb1d333e"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "78d24cf419138b543460f40509c8c1a168b52ca0")
  (package! github-review :pin "d0c8234cf523818513f892f30153210606abb6be"))
