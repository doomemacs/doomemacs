;; -*- no-byte-compile: t; -*-
;;; lang/nix/packages.el

(package! nix-mode)
(package! nix-update :recipe (:fetcher github :repo "jwiegley/nix-update-el"))

(when (featurep! :completion company)
  (package! company-nixos-options))
