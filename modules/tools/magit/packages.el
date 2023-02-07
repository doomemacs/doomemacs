;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "0ef98ef51811807952a4c3c677cbf3dfb269de2e")
  (package! compat :pin "7ca7d300d1d256f674f83932d2918d8e70cd28f6")
  (when (modulep! +forge)
    (package! forge :pin "ce212f8f95838889c51d0327eb8c3979bec6665c")
    (package! code-review
      :recipe (:files ("graphql" "code-review*.el"))
      :pin "95b36ec8e7935f96b2f3c6c9086d49cf4a72cbff"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "c5030cc27c7c1a48db52b0134bf2648a59a43176"))
