;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy :pin "e92cb37457b43336b765630dbfbea8ba4be601fa")
(package! drag-stuff :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
(package! link-hint :pin "09ba5727d8ba4a2e5d4f5ce924aaebbc7478ff13")

(unless (featurep! :editor evil)
  (package! expand-region :pin "ea6b4cbb9985ddae532bd2faf9bb00570c9f2781"))
