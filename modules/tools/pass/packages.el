;; -*- no-byte-compile: t; -*-
;;; tools/pass/packages.el

(package! pass :pin "a095d24cf06a7b0fbc3add480c101304a91cf788")
(package! password-store :pin "f152064da9832d6d3d2b4e75f43f63bf2d50716f")
(package! password-store-otp :pin "04998c8578a060ab4a4e8f46f2ee0aafad4ab4d5")

;; an older version of `auto-source-pass' is built into Emacs 26+, so we must
;; install the new version directly from the source and with a psuedonym.
(package! auth-source-pass
  :recipe (:host github :repo "DamienCassou/auth-password-store")
  :pin "aa7f17116ec3f760eb414d655ba20016b11a4a0e")

(when (featurep! :completion ivy)
  (package! ivy-pass :pin "5b523de1151f2109fdd6a8114d0af12eef83d3c5"))
(when (featurep! :completion helm)
  (package! helm-pass :pin "ed5798f2d83937575e8f23fde33323bca9e85131"))
