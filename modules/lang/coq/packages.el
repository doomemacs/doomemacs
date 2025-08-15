;; -*- no-byte-compile: t; -*-
;;; lang/coq/packages.el

(package! proof-general
  :pin "af2e7b9a4e938ec485915b8a4d1ea918438758a0"
  ;; REVIEW: Remove when ProofGeneral/PG#771 is fixed. Also see #8169.
  :recipe (:build (:not autoloads)))
(package! company-coq :pin "5affe7a96a25df9101f9e44bac8a828d8292c2fa")
