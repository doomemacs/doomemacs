;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "068f8389743b96d31069bb891b6cc26810bf50d9")
  (when (featurep! +forge)
    (package! forge :pin "05ef02913004826165c383bd6d2ff6574542b76c"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "78d24cf419138b543460f40509c8c1a168b52ca0")
  (package! github-review :pin "db723740e02348c0760407e532ad667ef89210ec"))
