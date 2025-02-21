;; -*- no-byte-compile: t; -*-
;;; lang/coq/packages.el

(package! proof-general
  :pin "e0ec3db200f8ed465ec533b6409e64e3dfb6a9b0"
  ;; REVIEW: Remove when ProofGeneral/PG#771 is fixed. Also see #8169.
  :recipe (:build (:not autoloads)))
(package! company-coq :pin "5affe7a96a25df9101f9e44bac8a828d8292c2fa")
