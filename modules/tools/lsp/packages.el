;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "d3e44d33b76ef86e9190105d1153bcad2b49d1de")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "9b490eb384d34ffc60c16cf16fe725ce5c72303a"))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :pin "18d0c9869585e6a9ea5c40678f266cf7f5bb2d2e")))
  (package! lsp-mode :pin "d28dd6b7e39506c11e0cb86de69b04a0f1963f9a")
  (package! lsp-ui :pin "f0edfac7b3736fcab617cbeb07e465c9153ae68b")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "6b2a625f08fb096a35faebf3c3ea0c8b295bdacd"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "e740efb2abbc0ffd43f6dbcdb4527bc55723b842"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "68583913168bf66fd4d542b2517a2dcab19c447c")))
