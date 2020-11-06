;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "2fb3bf782ccf5652b98f8de989f014749473eacf")
  (when (featurep! +forge)
    (package! forge :pin "031e4f06b2bff66375e53ea96f8eb3dfbdf391d9"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "a0e5d1f3c7dfcb4f18c1b0d57f1746a4872df5c6")
  (package! github-review :pin "e2123cce391bfd9d947ba2934b5d655ab1e90b6d")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "88dc26ce59dbf4acb4e2891c79c4bd329553ba56")))
