;; -*- no-byte-compile: t; -*-
;;; tools/pass/packages.el

(package! pass :pin "919d8e3826")
(package! password-store :pin "88936b11af")
(package! password-store-otp :pin "04998c8578")

;; an older version of `auto-source-pass' is built into Emacs 26+, so we must
;; install the new version directly from the source and with a psuedonym.
(package! auth-source-pass
  :recipe (:host github :repo "DamienCassou/auth-password-store")
  :pin "ff4940c647")

(when (featurep! :completion ivy)
  (package! ivy-pass :pin "5b523de115"))
(when (featurep! :completion helm)
  (package! helm-pass :pin "ed5798f2d8"))
