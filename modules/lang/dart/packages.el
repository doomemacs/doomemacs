;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "f82ff052309125b93d19bdd3f619266f908f43ce")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! dart-ts-mode
    :recipe (:host github
             :repo "50ways2sayhard/dart-ts-mode")
    :pin "ab87873f25f7e0cc8d22daa2501aae141dbe98ad"))

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-dart :pin "34e2a1191f723792d5f366b314cd6b07de4f1566"))

(when (modulep! +flutter)
  (package! flutter :pin "e71235d400787d977da7ed792709437899c2a03c")
  (package! hover :pin "1b380fa3951d78a9a9eda28a4bcb5a3536a100b9"))
