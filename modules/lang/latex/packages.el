;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex)
;; (package! auctex-latexmk)

(when (featurep! :completion company)
  (package! company-auctex))
(when (featurep! :completion ivy)
  (package! ivy-bibtex))
(when (featurep! :completion helm)
  (package! helm-bibtex))
