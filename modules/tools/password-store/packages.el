;; -*- no-byte-compile: t; -*-
;;; tools/password-store/packages.el

(package! pass)
(package! password-store)
(package! password-store-otp)

;; `auto-source-pass' is built into Emacs 26+
(unless EMACS26+
  (package! auth-source-pass))

(when (featurep! :completion ivy)
  (package! ivy-pass))
(when (featurep! :completion helm)
  (package! helm-pass))
