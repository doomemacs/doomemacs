;; -*- no-byte-compile: t; -*-
;;; lang/purescript/packages.el

;; As far as I can tell, at the moment, flycheck-purescript does not work well
;; due to expecting the compiler to be psc rather than purs. However, one of
;; purescript-mode or psc-ide seems to handle flycheck, so it might be
;; unnecessary altogether.
;;(package! flycheck-purescript :pin "30f0435d5e")

(package! psc-ide :pin "663f4e2cf9cbafdd4b9a60c34346596e2a40c87c")
(package! purescript-mode :pin "154ad16b61fb9dec83a6c863ffaf92638278f00f")
