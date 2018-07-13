;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex)
(package! adaptive-wrap)

;; Optional module features:

(when (featurep! +latexmk)
  (package! auctex-latexmk))

(when (featurep! +preview-pane)
  (package! latex-preview-pane))

;; Features according to other user selected options

(when (featurep! :completion company)
  (package! company-auctex)
  (package! company-reftex)
  (package! company-math))
(when (featurep! :completion ivy)
  (package! ivy-bibtex))
(when (featurep! :completion helm)
  (package! helm-bibtex))

