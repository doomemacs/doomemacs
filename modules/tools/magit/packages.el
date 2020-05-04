;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "d27d6e467857ed4a78c7cf7d609561df789e2a6c")
  (when (featurep! +forge)
    (package! forge :pin "e2da80660a0550f613400ce3b238025589800417"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "a0e5d1f3c7dfcb4f18c1b0d57f1746a4872df5c6")
  (package! github-review :pin "50c6bcc7cf4d7193577b3f74eea4dd72f2b7795b")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "253c644807013fe92429acdef418748794b8f254")))
