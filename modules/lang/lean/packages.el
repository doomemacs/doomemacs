;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(package! lean-mode :pin "cc1f5fadf8e9ae08aa25828985edc97df04d94a7")

(when (featurep! :completion company)
  (package! company-lean :pin "cc1f5fadf8e9ae08aa25828985edc97df04d94a7"))
