;; -*- no-byte-compile: t; -*-
;;; tools/pass/packages.el

(package! pass)
(package! password-store)
(package! password-store-otp)

;; an older version of `auto-source-pass' is built into Emacs 26+, so we must
;; install the new version directly from the source and with a psuedonym.
(package! auth-source-pass
  :recipe (:host github :repo "DamienCassou/auth-password-store"))

(when (featurep! :completion ivy)
  (package! ivy-pass))
(when (featurep! :completion helm)
  (package! helm-pass))
