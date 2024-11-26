;; -*- no-byte-compile: t; -*-
;;; lang/graphql/packages.el

(package! graphql-mode :pin "c3baca92809e6f6c8dd2596a413b277f3b0160ea")
(package! graphql-doc :pin "17755a2466a1acef68eac664093fcd13cd51494a")
(unless (modulep! +lsp)
  (package! company-graphql
    :recipe (:host github :repo "thaenalpha/company-graphql")
    :pin "aed9f5109e877944a895d08fc08bad103f03096b"))
