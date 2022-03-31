;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (featurep! :completion ivy)
  (package! bibtex-completion :pin "db73156576ee3e4ea9d7fb06a20e3cc2c8225eaf")
  (package! ivy-bibtex :pin "db73156576ee3e4ea9d7fb06a20e3cc2c8225eaf"))
(when (featurep! :completion helm)
  (package! bibtex-completion :pin "db73156576ee3e4ea9d7fb06a20e3cc2c8225eaf")
  (package! helm-bibtex :pin "db73156576ee3e4ea9d7fb06a20e3cc2c8225eaf"))
(when (featurep! :completion vertico)
  (package! citar :pin "ee98b94f7f80ae3c11b901ff1bc6212f9e5701a5"))

(package! citeproc :pin "ba49516265fa24b138346c4918d39d19b4de8a62")
