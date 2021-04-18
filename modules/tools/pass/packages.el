;; -*- no-byte-compile: t; -*-
;;; tools/pass/packages.el

(package! pass :pin "5651da53137db9adcb125b4897c2fe27eeb4368d")
(package! password-store :pin "918992c19231b33b3d4a3288a7288a620e608cb4")
(package! password-store-otp :pin "04998c8578a060ab4a4e8f46f2ee0aafad4ab4d5")

;; an older version of `auto-source-pass' is built into Emacs 26+, so we must
;; install the new version directly from the source and with a psuedonym.
(package! auth-source-pass
  :recipe (:host github :repo "DamienCassou/auth-password-store")
  :pin "468bba286fc20d739ed7724ec884357907ac8bda")

(when (featurep! :completion ivy)
  (package! ivy-pass :pin "5b523de1151f2109fdd6a8114d0af12eef83d3c5"))
(when (featurep! :completion helm)
  (package! helm-pass :pin "4ce46f1801f2e76e53482c65aa0619d427a3fbf9"))
