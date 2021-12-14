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
