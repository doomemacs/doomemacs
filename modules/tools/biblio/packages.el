;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "b85662081de98077f13f1a9fac03764702325d28")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "b85662081de98077f13f1a9fac03764702325d28"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "b85662081de98077f13f1a9fac03764702325d28"))
(when (featurep! :completion vertico)
  (package! citar :pin "fd33f5c4f7981036a969b5ca8aaf42380848ab32"))
