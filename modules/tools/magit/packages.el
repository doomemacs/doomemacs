;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "8de6ecf5c5")
  (when (featurep! +forge)
    (package! forge :pin "e2da80660a"))
  (package! magit-gitflow :pin "cc41b561ec")
  (package! magit-todos :pin "a0e5d1f3c7")
  (package! github-review :pin "50c6bcc7cf")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "253c644807")))
