;; -*- no-byte-compile: t; -*-
;;; lang/coq/packages.el

(package! proof-general
  :pin "6946aa5bebbd5dbbb2985d22325a03290fb6b79f"
  ;; REVIEW: Remove when ProofGeneral/PG#771 is fixed. Also see #8169.
  :recipe (:build (:not autoloads)))
(package! company-coq :pin "78ed04ce39e925232a556d2077718cc7b215469c")
