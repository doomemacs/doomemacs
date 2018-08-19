;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex)
(package! adaptive-wrap)
(package! latex-preview-pane)

;; Optional module features:

(when (featurep! +latexmk)
  (package! auctex-latexmk))

;; Features according to other user selected options

(when (featurep! :completion company)
  (package! company-auctex)
  (package! company-reftex)
  (package! company-math))
(when (featurep! :completion ivy)
  (package! ivy-bibtex))
(when (featurep! :completion helm)
  (package! helm-bibtex))

