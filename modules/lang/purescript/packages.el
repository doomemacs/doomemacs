;; -*- no-byte-compile: t; -*-
;;; lang/purescript/packages.el

;; As far as I can tell, at the moment, flycheck-purescript does not work well
;; due to expecting the compiler to be psc rather than purs. However, one of
;; purescript-mode or psc-ide seems to handle flycheck, so it might be
;; unnecessary altogether.
;;(package! flycheck-purescript :pin "30f0435d5e2715053c8c6170b2bce2ae462ac819")

(package! psc-ide :pin "2a9394422da317b54aa1da021aea6cded19004c1")
(package! purescript-mode :pin "8db1d0243c03da31adac4d7c5287407a4df6aff2")
