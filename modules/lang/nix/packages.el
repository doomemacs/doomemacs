;; -*- no-byte-compile: t; -*-
;;; lang/nix/packages.el

(package! nix-mode :pin "20ee8d88900b169831d6b0783bd82d2625e940c7")
(package! nix-update :pin "aab70a38165575a9cb41726f1cc67df60fbf2832")

(when (modulep! :completion company)
  (package! company-nixos-options :pin "053a2d5110ce05b7f99bcc2ac4804b70cbe87916"))

(when (modulep! :completion helm)
  (package! helm-nixos-options :pin "053a2d5110ce05b7f99bcc2ac4804b70cbe87916"))
