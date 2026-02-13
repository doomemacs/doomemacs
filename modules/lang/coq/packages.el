;; -*- no-byte-compile: t; -*-
;;; lang/coq/packages.el

(package! proof-general
  :pin "75c13f91b6eb40b8855dfe8ac55f8f7dac876caa"
  ;; REVIEW: Remove when ProofGeneral/PG#771 is fixed. Also see #8169.
  :recipe (:build (:not autoloads)))
(package! company-coq :pin "78ed04ce39e925232a556d2077718cc7b215469c")
