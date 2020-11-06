;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "12f8809aac3a13dd11a1c664a13f789005f7a199")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "12f8809aac3a13dd11a1c664a13f789005f7a199"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "12f8809aac3a13dd11a1c664a13f789005f7a199"))
