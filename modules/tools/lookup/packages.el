;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

;; HACK `dumb-jump' uses the `helm-build-sync-source' macro, but this requires
;;      helm be loaded before `dumb-jump' is byte-compiled during installation.
;;      To ensure this, we declare helm before dumb-jump.
(when (featurep! :completion helm)
  (package! helm))

;;
(package! dumb-jump :pin "8f70acbe164553b225476fed55019ecddcf0bbd6")
(when (featurep! :completion ivy)
  (package! ivy-xref :pin "3d4c35fe2b243d948d8fe02a1f0d76a249d63de9"))
(when (featurep! :completion helm)
  (package! helm-xref :pin "23f1174cfca7667d95828dcd388c655a4a9c877d"))

;; For dictionary and online lookup
(package! request :pin "f3a5b4352e9f444ace2a332939abff504b573887")

(when (featurep! +docsets)
  (package! dash-docs :pin "dafc8fc9f1ddb2e4e39e0b8d066c42d5d7ce8d06")
  (when (featurep! :completion helm)
    (package! helm-dash :pin "7f853bd34da666f0e9a883011c80f451b06f6c59"))
  (when (featurep! :completion ivy)
    (package! counsel-dash :pin "370d5f6f14b5294d0eb717f7b2a6a8e93df1ed24")))

(when (featurep! +dictionary)
  (if IS-MAC
      (package! osx-dictionary :pin "4d4cc19fdd4ac8408bd5acc7694e7a7096b1e3b3")
    (package! define-word :pin "6e4a427503aef096484f88332962c346cdd10847")
    (package! powerthesaurus :pin "93036d3b111925ebc34f747ff846cb0b8669b92e")
    (when (featurep! +offline)
      (package! wordnut :pin "feac531404041855312c1a046bde7ea18c674915")
      (package! synosaurus :pin "14d34fc92a77c3a916b4d58400424c44ae99cd81"))))
