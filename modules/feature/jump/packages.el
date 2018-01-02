;; -*- no-byte-compile: t; -*-
;;; feature/jump/packages.el

(package! dumb-jump)
(when (featurep! :completion ivy)
  (package! ivy-xref))
(when (featurep! :completion helm)
  (package! helm-xref))
