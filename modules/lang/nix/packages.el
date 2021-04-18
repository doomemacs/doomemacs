;; -*- no-byte-compile: t; -*-
;;; lang/nix/packages.el

(package! nix-mode :pin "0023fc5b100ec0c939ffe699d1a7d1afcf1f417a")
(package! nix-update :pin "fc6c39c2da3fcfa62f4796816c084a6389c8b6e7")

(when (featurep! :completion company)
  (package! company-nixos-options :pin "053a2d5110ce05b7f99bcc2ac4804b70cbe87916"))

(when (featurep! :completion helm)
  (package! helm-nixos-options :pin "053a2d5110ce05b7f99bcc2ac4804b70cbe87916"))
