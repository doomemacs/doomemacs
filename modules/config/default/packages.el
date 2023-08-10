;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy :pin "955c8dedd68c74f3cf692c1249513f048518c4c9")
(package! drag-stuff :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
(package! link-hint :pin "36ce929331f2838213bcaa1145ece4b73ce84afe")

(unless (modulep! :editor evil)
  (package! expand-region :pin "b70feaa644310dc2d599dc277cd20a1f2b6446ac"))
