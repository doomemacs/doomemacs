;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "9f6ea920a49457d85096caa0e61f086a42b2908e")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "9f6ea920a49457d85096caa0e61f086a42b2908e"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "9f6ea920a49457d85096caa0e61f086a42b2908e"))
