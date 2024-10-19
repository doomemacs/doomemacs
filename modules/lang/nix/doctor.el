;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/nix/doctor.el

(unless (executable-find "nix")
  (warn! "Couldn't find the nix package manager. nix-mode won't work."))

(when (require 'nix-mode nil t)
  (unless (executable-find nix-nixfmt-bin)
    (warn! (concat "Couldn't find " nix-nixfmt-bin ". nix-format-buffer won't work."))))

(assert! (or (modulep! -tree-sitter)
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")
