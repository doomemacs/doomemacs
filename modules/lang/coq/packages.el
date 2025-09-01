;; -*- no-byte-compile: t; -*-
;;; lang/coq/packages.el

(package! proof-general
  :pin "fbb2878e49483181f6687b8ca15ecf9a597ff947"
  ;; REVIEW: Remove when ProofGeneral/PG#771 is fixed. Also see #8169.
  :recipe (:build (:not autoloads)))
(package! company-coq :pin "78ed04ce39e925232a556d2077718cc7b215469c")
