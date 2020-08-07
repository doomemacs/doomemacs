;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

;; HACK `dumb-jump' uses the `helm-build-sync-source' macro, but this requires
;;      helm be loaded before `dumb-jump' is byte-compiled during installation.
;;      To ensure this, we declare helm before dumb-jump.
(when (featurep! :completion helm)
  (package! helm))

;;
(package! dumb-jump :pin "b81573184c97fbc6181d0a4fe408caa23c0d621b")
(when (featurep! :completion ivy)
  (package! ivy-xref :pin "3d4c35fe2b243d948d8fe02a1f0d76a249d63de9"))
(when (featurep! :completion helm)
  (package! helm-xref :pin "6b4a8bd91f5eaf82f51bd31b03f6587387fe6983"))

;; For dictionary and online lookup
(package! request :pin "d02d1347ffdf138cffd380cbeac62ac8732036ef")

(when (featurep! +docsets)
  (package! dash-docs :pin "dafc8fc9f1ddb2e4e39e0b8d066c42d5d7ce8d06")
  (when (featurep! :completion helm)
    (package! helm-dash :pin "7f853bd34da666f0e9a883011c80f451b06f6c59"))
  (when (featurep! :completion ivy)
    (package! counsel-dash :pin "370d5f6f14b5294d0eb717f7b2a6a8e93df1ed24")))

(when (featurep! +dictionary)
  (if IS-MAC
      (package! osx-dictionary :pin "1b79ff64c72485cb078db9ab7ee3256b11a99f4b")
    (package! define-word :pin "08c71b1ff4fd07bf0c78d1fcf77efeaafc8f7443")
    (package! powerthesaurus :pin "93036d3b111925ebc34f747ff846cb0b8669b92e")
    (when (featurep! +offline)
      (package! wordnut :pin "feac531404041855312c1a046bde7ea18c674915")
      (package! synosaurus :pin "14d34fc92a77c3a916b4d58400424c44ae99cd81"))))
