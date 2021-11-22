;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (featurep! :completion ivy)
  (package! bibtex-completion :pin "bb47f355b0da8518aa3fb516019120c14c8747c9")
  (package! ivy-bibtex :pin "bb47f355b0da8518aa3fb516019120c14c8747c9"))
(when (featurep! :completion helm)
  (package! bibtex-completion :pin "bb47f355b0da8518aa3fb516019120c14c8747c9")
  (package! helm-bibtex :pin "bb47f355b0da8518aa3fb516019120c14c8747c9"))
(when (featurep! :completion vertico)
  (package! citar :pin "41ec5d4d5d625f7d784b4de20d14b7bceaf1730c"))

(package! citeproc :pin "c8ff95862823cdff067e8cc9bb7f5ef537e8f1d9")
