;; -*- no-byte-compile: t; -*-
;;; lang/nix/packages.el

(package! nix-mode :pin "719feb7868fb567ecfe5578f6119892c771ac5e5")
(package! nix-update :pin "d67f4f7ba8c8ec43144600f5f970c5fd958fc2f7")
(package! nixos-options :pin "e241b58c9e6e24b9c2bdc708631db21a3ed2fe4b")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! nix-ts-mode :pin "319831712125e8c9e4f14b66def518d7a633685c"))

(when (modulep! :completion company)
  (package! company-nixos-options :pin "e241b58c9e6e24b9c2bdc708631db21a3ed2fe4b"))

(when (modulep! :completion helm)
  (package! helm-nixos-options :pin "e241b58c9e6e24b9c2bdc708631db21a3ed2fe4b"))
