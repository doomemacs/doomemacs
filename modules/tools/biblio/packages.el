;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "1bb81d77e08296a50de7ebfe5cf5b0c715b7f3d6")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "1bb81d77e08296a50de7ebfe5cf5b0c715b7f3d6"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "1bb81d77e08296a50de7ebfe5cf5b0c715b7f3d6"))
