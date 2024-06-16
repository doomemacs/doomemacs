;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy :pin "be612110cb116a38b8603df367942e2bb3d9bdbe")
(package! drag-stuff :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
(package! link-hint :pin "9153eafc776549376bb85d9ff555fef83aca8285")

(unless (modulep! :editor evil)
  (package! expand-region :pin "e8f4e0fe9c9a80a6a26e2b438502aba9a799d580"))
