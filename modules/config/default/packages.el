;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy :pin "cf95ba9582121a1c2249e3c5efdc51acd566d190")
(package! drag-stuff :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
(package! link-hint :pin "8d8f9505f87dc8a3b3baee7cb516f091072893a7")

(unless (featurep! :editor evil)
  (package! expand-region :pin "0fa7c2d349e40c0e1de0965acf0f0b77b7070451"))
