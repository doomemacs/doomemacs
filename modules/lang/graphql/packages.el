;; -*- no-byte-compile: t; -*-
;;; lang/graphql/packages.el

(package! graphql-mode :pin "9740e4027bd9313697d5cac5caaa5b15626ab1da")
(package! graphql-doc :pin "6ba7961fc9c5c9818bd60abce6ba9dfef2dad452")
(when (not (featurep! +lsp))
  (package! company-graphql :pin "757dfa45ad0cef9b9c362c8993d6474a2426c01c"))
