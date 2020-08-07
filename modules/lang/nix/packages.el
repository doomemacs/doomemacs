;; -*- no-byte-compile: t; -*-
;;; lang/nix/packages.el

(package! nix-mode :pin "a00b3f776de65b5af7571976a73213ca2a5c683e")
(package! nix-update :pin "fc6c39c2da3fcfa62f4796816c084a6389c8b6e7")

(when (featurep! :completion company)
  (package! company-nixos-options :pin "977b9a505ffc8b33b70ec7742f90e469b3168297"))

(when (featurep! :completion helm)
  (package! helm-nixos-options :pin "977b9a505ffc8b33b70ec7742f90e469b3168297"))
