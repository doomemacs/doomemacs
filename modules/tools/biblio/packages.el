;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "9f6ea920a49457d85096caa0e61f086a42b2908e")
(package! citeproc :pin "0857973409e3ef2ef0238714f2ef7ff724230d1c")

(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "9f6ea920a49457d85096caa0e61f086a42b2908e"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "9f6ea920a49457d85096caa0e61f086a42b2908e"))
<<<<<<< HEAD
<<<<<<< HEAD
(when (featurep! :completion vertico)
  (package! bibtex-actions :pin "6e3a194c3ab655693f8194be78542366755c58c9"))
=======
(when (featurep! :completion selectrum)
=======
(when (featurep! :completion vertico)
>>>>>>> 83e43eb01 (completion/selectrum -> completion/vertico, part 2)
  (package! bibtex-actions :pin "b1ddbb32373ac01b6bb46dfc4cdc143461e3c14c"))
>>>>>>> bcd371e83 (Bump bibtex-actions)
