;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "73530108957c1167e155df9658d843c3bac0d9c7")
  (when (featurep! +forge)
    (package! forge :pin "9e7d6835b7eb3283eee84998de99af38cd9a2028"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "60152d5c4e4b73e72e15f23ca16e8cc7734906bc")
  (package! github-review :pin "341b7a1352e4ee1f1119756360ac0714abbaf460"))
