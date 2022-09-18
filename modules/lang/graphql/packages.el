;; -*- no-byte-compile: t; -*-
;;; lang/graphql/packages.el

(package! graphql-mode :pin "9740e4027bd9313697d5cac5caaa5b15626ab1da")
(package! graphql-doc :pin "d37140267e0c426c7c18aff31900aa1650257394")
(unless (modulep! +lsp)
  (package! company-graphql
    :recipe (:host github :repo "thaenalpha/company-graphql")
    :pin "aed9f5109e877944a895d08fc08bad103f03096b"))
