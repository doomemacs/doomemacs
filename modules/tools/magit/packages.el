;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "1e40d0021790707f6e88debda04f6b14d9429586")
  (when (featurep! +forge)
    (package! forge :pin "f97bc47e9e2a2a6300dd267bdd67a88254f65aa7"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "60152d5c4e4b73e72e15f23ca16e8cc7734906bc")
  (package! github-review :pin "341b7a1352e4ee1f1119756360ac0714abbaf460"))
