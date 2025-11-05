;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy :pin "933d1f36cca0f71e4acb5fac707e9ae26c536264")
(package! link-hint :pin "8fda5dcb9caff5a3c49d22b82e570ac9e29af7dd")

(unless (modulep! :editor evil)
  (package! drag-stuff :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
  (package! expand-region :pin "351279272330cae6cecea941b0033a8dd8bcc4e8"))
