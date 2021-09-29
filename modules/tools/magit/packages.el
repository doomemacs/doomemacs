;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "f53148a569191bdbfb78d76f28481b91c60cb846")
  (when (featurep! +forge)
    (package! forge :pin "483baae56df8b1d8f1a9136512d23e106c831c0b"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "60152d5c4e4b73e72e15f23ca16e8cc7734906bc")
  (package! github-review :pin "341b7a1352e4ee1f1119756360ac0714abbaf460"))
