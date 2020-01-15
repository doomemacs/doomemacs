;; -*- no-byte-compile: t; -*-
;;; tools/pass/packages.el

(package! pass :pin "919d8e3826d556433ab67d4ee21a509d209d1baa")
(package! password-store :pin "88936b11aff49e48f79842e4628c55620e0ad736")
(package! password-store-otp :pin "04998c8578a060ab4a4e8f46f2ee0aafad4ab4d5")

;; an older version of `auto-source-pass' is built into Emacs 26+, so we must
;; install the new version directly from the source and with a psuedonym.
(package! auth-source-pass
  :recipe (:host github :repo "DamienCassou/auth-password-store")
  :pin "ff4940c647786914b3cbef69103d96a4ea334111")

(when (featurep! :completion ivy)
  (package! ivy-pass :pin "5b523de1151f2109fdd6a8114d0af12eef83d3c5"))
(when (featurep! :completion helm)
  (package! helm-pass :pin "ed5798f2d83937575e8f23fde33323bca9e85131"))
