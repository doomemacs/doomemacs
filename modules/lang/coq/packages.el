;; -*- no-byte-compile: t; -*-
;;; lang/coq/packages.el

(package! proof-general
  :pin "85cde55a86b2c3a67d8bbffa8de0d0276f724643"
  ;; REVIEW: Remove when ProofGeneral/PG#771 is fixed. Also see #8169.
  :recipe (:build (:not autoloads)))
(package! company-coq :pin "5affe7a96a25df9101f9e44bac8a828d8292c2fa")
