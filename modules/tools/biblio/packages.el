;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (featurep! :completion ivy)
  (package! bibtex-completion :pin "aa775340ba691d2322948bfdc6a88158568a1399")
  (package! ivy-bibtex :pin "aa775340ba691d2322948bfdc6a88158568a1399"))
(when (featurep! :completion helm)
  (package! bibtex-completion :pin "aa775340ba691d2322948bfdc6a88158568a1399")
  (package! helm-bibtex :pin "aa775340ba691d2322948bfdc6a88158568a1399"))
(when (featurep! :completion vertico)
  (package! citar :pin "51b30f2e4091a41243ae62cfbac53e7a579f3168"))
(package! citeproc :pin "538fed794c29acf81efee8a2674268bd3d7cc471")
(when (featurep! :lang org +ref)
  (package! org-ref :pin "429733150548a847966685680bca0a20ec3b1ad9"))
(when (featurep! :lang org +roam2)
  (package! org-roam-bibtex :pin "cf811abf273ad28d32caad3a93318f92da034556"))
