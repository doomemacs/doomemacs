;; -*- no-byte-compile: t; -*-
;;; lang/nix/packages.el

(package! nix-mode)

(when (featurep! :completion company)
  (package! company-nixos-options))
