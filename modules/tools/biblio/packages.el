;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (featurep! :completion ivy)
  (package! bibtex-completion :pin "aa775340ba691d2322948bfdc6a88158568a1399")
  (package! ivy-bibtex :pin "aa775340ba691d2322948bfdc6a88158568a1399"))
(when (featurep! :completion helm)
  (package! bibtex-completion :pin "aa775340ba691d2322948bfdc6a88158568a1399")
  (package! helm-bibtex :pin "aa775340ba691d2322948bfdc6a88158568a1399"))
(when (featurep! :completion vertico)
  (package! citar :pin "b24b5b94b7d5f6e7e96331b48a22e977e0013de1"))

(package! citeproc :pin "9f783967c1909cc741a691ee4e4021a952ffc4e1")
