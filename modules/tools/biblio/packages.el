;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "8d84d8beb3bfafaa060d7e956fcc569ecf010d9c")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "8d84d8beb3bfafaa060d7e956fcc569ecf010d9c"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "8d84d8beb3bfafaa060d7e956fcc569ecf010d9c"))
