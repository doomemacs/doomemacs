;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "773e9ebc74a258af2db395b01febfb652a42f3ab")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! dart-ts-mode
    :recipe (:host github
             :repo "50ways2sayhard/dart-ts-mode")
    :pin "ab87873f25f7e0cc8d22daa2501aae141dbe98ad"))

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-dart :pin "fd8239518026041a07305e88c15ad40faf3c3038"))

(when (modulep! +flutter)
  (package! flutter :pin "e71235d400787d977da7ed792709437899c2a03c")
  (package! hover :pin "1b380fa3951d78a9a9eda28a4bcb5a3536a100b9"))
