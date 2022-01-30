;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "4e29d5827cb77a7fbcff57033a099e23b8edd424")
  (when (featurep! +forge)
    (package! forge :pin "5586863f9862b05ee4bbdc53034de109be9dbf66"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "60152d5c4e4b73e72e15f23ca16e8cc7734906bc")
  (package! code-review :pin "ccc3795a72554439f230969322c0e3239252c193"
    :recipe (:files ("graphql" "code-review*.el"))))
