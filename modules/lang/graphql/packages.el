;; -*- no-byte-compile: t; -*-
;;; lang/graphql/packages.el

(package! graphql-mode :pin "ee49531935ede7a2c9597713e13a4c9d33ef2220")
(package! graphql-doc :pin "17755a2466a1acef68eac664093fcd13cd51494a")
(unless (modulep! +lsp)
  (package! company-graphql
    :recipe (:host github :repo "thaenalpha/company-graphql")
    :pin "aed9f5109e877944a895d08fc08bad103f03096b"))
(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! graphql-ts-mode :pin "e933f235408ea195762700fd07c2d828e8f09aac"))
