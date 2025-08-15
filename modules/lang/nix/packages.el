;; -*- no-byte-compile: t; -*-
;;; lang/nix/packages.el

(package! nix-mode :pin "719feb7868fb567ecfe5578f6119892c771ac5e5")
(package! nix-update :pin "77022ccd918d665acbb519b243e7e3dc5eae1c47")

(when (modulep! :completion company)
  (package! company-nixos-options :pin "053a2d5110ce05b7f99bcc2ac4804b70cbe87916"))

(when (modulep! :completion helm)
  (package! helm-nixos-options :pin "053a2d5110ce05b7f99bcc2ac4804b70cbe87916"))
