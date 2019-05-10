;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy)
(package! ace-link)

(unless (featurep! :editor evil)
  (package! expand-region))
