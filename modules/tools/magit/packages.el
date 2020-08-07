;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "798aff56457aef4d1896db8129ab56d08ae12066")
  (when (featurep! +forge)
    (package! forge :pin "2c487465d0b78ffe34252b47fcc06e27039330c4"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "a0e5d1f3c7dfcb4f18c1b0d57f1746a4872df5c6")
  (package! github-review :pin "fab440aeae4fbf6a8192fd11795052e9eb5d27d1")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "88dc26ce59dbf4acb4e2891c79c4bd329553ba56")))
