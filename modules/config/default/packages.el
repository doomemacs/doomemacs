;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(unless (featurep! :editor evil)
  (package! winum)
  (package! expand-region))
