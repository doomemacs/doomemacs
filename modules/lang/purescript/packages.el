;; -*- no-byte-compile: t; -*-
;;; lang/purescript/packages.el

;; As far as I can tell, at the moment, flycheck-purescript does not work well
;; due to expecting the compiler to be psc rather than purs. However, one of
;; purescript-mode or psc-ide seems to handle flycheck, so it might be
;; unnecessary altogether.
;;(package! flycheck-purescript)

(package! psc-ide)
(package! purescript-mode)

