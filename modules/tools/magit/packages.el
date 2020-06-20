;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "0746bf1bacfe896d3917ccc19c7fb9d95c18b1e9")
  (when (featurep! +forge)
    (package! forge :pin "048efbba83b1df591de0487202ff968250ea4fc5"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "a0e5d1f3c7dfcb4f18c1b0d57f1746a4872df5c6")
  (package! github-review :pin "50c6bcc7cf4d7193577b3f74eea4dd72f2b7795b")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "88dc26ce59dbf4acb4e2891c79c4bd329553ba56")))
