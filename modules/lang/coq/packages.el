;; -*- no-byte-compile: t; -*-
;;; lang/coq/packages.el

(package! proof-general
  :pin "d6689469298b4140dc1f0f8b0ff7e8f937041ffe"
  ;; REVIEW: Remove when ProofGeneral/PG#771 is fixed. Also see #8169.
  :recipe (:build (:not autoloads)))
(package! company-coq :pin "5affe7a96a25df9101f9e44bac8a828d8292c2fa")
