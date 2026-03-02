;; -*- no-byte-compile: t; -*-
;;; lang/coq/packages.el

(package! proof-general
  :pin "75c13f91b6eb40b8855dfe8ac55f8f7dac876caa"
  ;; REVIEW: Remove when ProofGeneral/PG#771 is fixed. Also see #8169.
  :recipe (:build (:not autoloads)))
(package! company-coq :pin "1fc1d8f2d56e460b33c6d41a659488dce7b214f9")
