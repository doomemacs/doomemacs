;; -*- no-byte-compile: t; -*-
;;; checkers/syntax/packages.el

(package! flycheck :pin "01396a5eff9fa494285e0d3139838231c05e3948")
(package! flycheck-popup-tip :pin "ef86aad907f27ca076859d8d9416f4f7727619c6")
(when (featurep! +childframe)
  (package! flycheck-posframe :pin "24fd9b3d81eab8dd850c504ae46a5c5f11a46ee0"))

;; TODO flymake?
