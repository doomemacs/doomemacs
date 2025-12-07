;; -*- no-byte-compile: t; -*-
;;; lang/nix/packages.el

(package! nix-mode :pin "719feb7868fb567ecfe5578f6119892c771ac5e5")
(package! nix-update :pin "d67f4f7ba8c8ec43144600f5f970c5fd958fc2f7")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! nix-ts-mode :pin "706bbfaf7f344d7c5914bf7aa5beb2d904eb53b3"))

(when (modulep! :completion company)
  (package! company-nixos-options :pin "053a2d5110ce05b7f99bcc2ac4804b70cbe87916"))

(when (modulep! :completion helm)
  (package! helm-nixos-options :pin "053a2d5110ce05b7f99bcc2ac4804b70cbe87916"))
