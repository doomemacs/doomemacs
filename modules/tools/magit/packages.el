;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "b4be194de1edb3d64ebf2abf199c6ec531904319")
  (when (featurep! +forge)
    (package! forge :pin "eed613db8880be8c0b5d564f4d0a2aebb5a64db9"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "67fd80c2f10aec4d5b2a24b5d3d53c08cc1f05dc")
  (package! code-review :pin "85ab2080e489b4ca01c787f5a316ade02a4ee877"
    :recipe (:files ("graphql" "code-review*.el"))))
