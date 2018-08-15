;; -*- no-byte-compile: t; -*-
;;; lang/coq/packages.el

(package! proof-general :recipe (:fetcher github :repo "ProofGeneral/PG" :files ("*")))

(package! company-coq)
