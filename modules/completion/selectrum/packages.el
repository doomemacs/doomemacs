;; -*- no-byte-compile: t; -*-
;;; completion/selectrum/packages.el

(package! selectrum)
(when (featurep! +prescient)
  (package! selectrum-prescient))

(package! consult)
(package! consult-flycheck)
(package! embark)
(package! embark-consult)
(package! marginalia)
