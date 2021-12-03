;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (featurep! :completion ivy)
  (package! bibtex-completion :pin "aa775340ba691d2322948bfdc6a88158568a1399")
  (package! ivy-bibtex :pin "aa775340ba691d2322948bfdc6a88158568a1399"))
(when (featurep! :completion helm)
  (package! bibtex-completion :pin "aa775340ba691d2322948bfdc6a88158568a1399")
  (package! helm-bibtex :pin "aa775340ba691d2322948bfdc6a88158568a1399"))
(when (featurep! :completion vertico)
  (package! citar :pin "7740300831af16f4c2bbc4012fcc6a21f1f9a809"))

(package! citeproc :pin "2e7df666bfeed92178d20c5851a2945ed5760664")
