;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "577f16da3072a0ae85fba6b25a36a971a61ec6c2")
  (when (featurep! +forge)
    (package! forge :pin "f4c95dd0e633b3dd332d6f91122c864ab3640912"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "78d24cf419138b543460f40509c8c1a168b52ca0")
  (package! github-review :pin "341b7a1352e4ee1f1119756360ac0714abbaf460"))
