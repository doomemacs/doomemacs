;; -*- no-byte-compile: t; -*-
;;; lang/nix/packages.el

(package! nix-mode :pin "5b5961780f")
(package! nix-update :pin "fc6c39c2da")

(when (featurep! :completion company)
  (package! company-nixos-options :pin "977b9a505f"))

(when (featurep! :completion helm)
  (package! helm-nixos-options :pin "977b9a505f"))
