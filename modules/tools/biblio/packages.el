;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "94807a3d3419f90b505eddc3272e244475eeb4f2")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "94807a3d3419f90b505eddc3272e244475eeb4f2"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "94807a3d3419f90b505eddc3272e244475eeb4f2"))
