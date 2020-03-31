;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

;; HACK `dumb-jump' uses the `helm-build-sync-source' macro, but this requires
;;      helm be loaded before `dumb-jump' is byte-compiled during installation.
;;      To ensure this, we declare helm before dumb-jump.
(when (featurep! :completion helm)
  (package! helm))

;;
(package! dumb-jump :pin "e8e9b0c2d1")
(when (featurep! :completion ivy)
  (package! ivy-xref :pin "3d4c35fe2b"))
(when (featurep! :completion helm)
  (package! helm-xref :pin "6b4a8bd91f"))

;; For dictionary and online lookup
(package! request :pin "216d570a58")

(when (featurep! +docsets)
  (package! dash-docs :pin "111fd9b970")
  (when (featurep! :completion helm)
    (package! helm-dash :pin "7f853bd34d"))
  (when (featurep! :completion ivy)
    (package! counsel-dash :pin "370d5f6f14")))

(when (featurep! +dictionary)
  (if IS-MAC
      (package! osx-dictionary :pin "1b79ff64c7")
    (package! define-word :pin "d8c76d503b")
    (package! powerthesaurus :pin "81a262ec0c")
    (when (featurep! +offline)
      (package! wordnut :pin "feac531404")
      (package! synosaurus :pin "14d34fc92a"))))
