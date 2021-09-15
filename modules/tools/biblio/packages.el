;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "b85662081de98077f13f1a9fac03764702325d28")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "b85662081de98077f13f1a9fac03764702325d28"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "b85662081de98077f13f1a9fac03764702325d28"))
(when (featurep! :completion vertico)
  (package! bibtex-actions :pin "08c6ca0e5b736de50a4d1db5a00ce01b4c2093eb"))
