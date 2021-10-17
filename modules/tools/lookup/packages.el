;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

;; HACK `dumb-jump' uses the `helm-build-sync-source' macro, but this requires
;;      helm be loaded before `dumb-jump' is byte-compiled during installation.
;;      To ensure this, we declare helm before dumb-jump.
(when (featurep! :completion helm)
  (package! helm))

;;
(package! dumb-jump :pin "f3176fbf9c11b94cf05bd8279399d9536115ff3c")
(when (featurep! :completion ivy)
  (package! ivy-xref :pin "a82e8e117d2dd62c28b6a3e3d6e4cfb11c0bda38"))
(when (featurep! :completion helm)
  (package! helm-xref :pin "ea0e4ed8a9baf236e4085cbc7178241f109a53fa"))

;; For dictionary and online lookup
(package! request :pin "68003b3f859724de621d0e5a8b0aae51ce708d1e")

(when (featurep! +docsets)
  (package! dash-docs :pin "29848b6b347ac520f7646c200ed2ec36cea3feda")
  (when (featurep! :completion helm)
    (package! helm-dash :pin "7f853bd34da666f0e9a883011c80f451b06f6c59"))
  (when (featurep! :completion ivy)
    (package! counsel-dash :pin "370d5f6f14b5294d0eb717f7b2a6a8e93df1ed24")))

(when (featurep! +dictionary)
  (if IS-MAC
      (package! osx-dictionary :pin "1a4479d9f44ef1e6e5f7643c172c32f6fe6cce21")
    (package! define-word :pin "6e4a427503aef096484f88332962c346cdd10847")
    (package! powerthesaurus :pin "93036d3b111925ebc34f747ff846cb0b8669b92e")
    (when (featurep! +offline)
      (package! wordnut :pin "feac531404041855312c1a046bde7ea18c674915")
      (package! synosaurus :pin "14d34fc92a77c3a916b4d58400424c44ae99cd81"))))
