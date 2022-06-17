;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (featurep! :completion ivy)
  (package! bibtex-completion :pin "ce8c17690ddad73d01531084b282f221f8eb6669")
  (package! ivy-bibtex :pin "ce8c17690ddad73d01531084b282f221f8eb6669"))
(when (featurep! :completion helm)
  (package! bibtex-completion :pin "ce8c17690ddad73d01531084b282f221f8eb6669")
  (package! helm-bibtex :pin "ce8c17690ddad73d01531084b282f221f8eb6669"))
(when (featurep! :completion vertico)
  (package! citar :pin "dd028c6a4d2fb1c7b89082dffa15be01637765c5"))

(package! citeproc :pin "ba49516265fa24b138346c4918d39d19b4de8a62")
