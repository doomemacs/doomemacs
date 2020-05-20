;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "b1b2683f6012ff3bc29bd9cbe562246477f1523f")
  (when (featurep! +forge)
    (package! forge :pin "09bf8adc9c9afb492632e612f51f39e1cc15fca0"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "a0e5d1f3c7dfcb4f18c1b0d57f1746a4872df5c6")
  (package! github-review :pin "50c6bcc7cf4d7193577b3f74eea4dd72f2b7795b")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "253c644807013fe92429acdef418748794b8f254")))
