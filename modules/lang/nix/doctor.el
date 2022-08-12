;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/nix/doctor.el

(unless (executable-find "nix")
  (warn! "Couldn't find the nix package manager. nix-mode won't work."))

(unless (executable-find "nixfmt")
  (warn! "Couldn't find nixfmt. nix-format-buffer won't work."))

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")
