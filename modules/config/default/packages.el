;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(unless (featurep! :feature evil)
  (package! winum)
  (package! expand-region))
