;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

;; HACK `dumb-jump' uses the `helm-build-sync-source' macro, but this requires
;;      helm be loaded before `dumb-jump' is byte-compiled during installation.
;;      To ensure this, we declare helm before dumb-jump.
(when (modulep! :completion helm)
  (package! helm))

;;
(package! dumb-jump :pin "0a783d1db610ff1dc4e1b7869589cff16ff92f7a")
(when (modulep! :completion ivy)
  (package! ivy-xref :pin "a82e8e117d2dd62c28b6a3e3d6e4cfb11c0bda38"))
(when (modulep! :completion helm)
  (package! helm-xref :pin "ea0e4ed8a9baf236e4085cbc7178241f109a53fa"))

;; For dictionary and online lookup
(package! request :pin "01e338c335c07e4407239619e57361944a82cb8a")

(when (modulep! +docsets)
  (package! dash-docs :pin "29848b6b347ac520f7646c200ed2ec36cea3feda")
  (when (modulep! :completion helm)
    (package! helm-dash :pin "7f853bd34da666f0e9a883011c80f451b06f6c59"))
  (when (modulep! :completion ivy)
    (package! counsel-dash :pin "8decb980f111ebe7027361ee252279a9076da261")))

(when (modulep! +dictionary)
  (if IS-MAC
      (package! osx-dictionary :pin "0715e5a3ac659df32a0f0fabfbbeef0228fbd9a9")
    (package! define-word :pin "31a8c67405afa99d0e25e7c86a4ee7ef84a808fe")
    (package! powerthesaurus :pin "88bc5229cba1604c8f74db0a1456d99259d538cc")
    (when (modulep! +offline)
      (package! wordnut :pin "feac531404041855312c1a046bde7ea18c674915")
      (package! synosaurus :pin "14d34fc92a77c3a916b4d58400424c44ae99cd81"))))
