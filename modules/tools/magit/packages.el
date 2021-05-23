;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "e37882710bd91d95bac967d342e5aea846cce783")
  (when (featurep! +forge)
    (package! forge :pin "37aa4e4b82a99246b3551daee6104dc1d192174a"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "78d24cf419138b543460f40509c8c1a168b52ca0")
  (package! github-review :pin "341b7a1352e4ee1f1119756360ac0714abbaf460"))
