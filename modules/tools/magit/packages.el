;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "30b0debaaadadec6103a8d7eab92322fd9d30a15")
  (when (modulep! +forge)
    (package! forge :pin "ce212f8f95838889c51d0327eb8c3979bec6665c")
    (package! code-review
      :recipe (:files ("graphql" "code-review*.el"))
      :pin "26f426e99221a1f9356aabf874513e9105b68140"))
  (package! magit-todos :pin "c5030cc27c7c1a48db52b0134bf2648a59a43176"))
