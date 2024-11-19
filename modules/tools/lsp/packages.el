;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "81eb273965be82cd56d7502cccd68b5231a8fbab")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "c5f87d92448cd9c22a33ebd1feb54ca2fb89afa8"))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :pin "18d0c9869585e6a9ea5c40678f266cf7f5bb2d2e")))
  (package! lsp-mode :pin "32628135efca935203624ff9526bb0bcd42f714c")
  (package! lsp-ui :pin "072bb29152038518c2478813b82c8d04d07df84c")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "9ecf4dd9b1207109802bd1882aa621eb1c385106"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "e740efb2abbc0ffd43f6dbcdb4527bc55723b842"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "68583913168bf66fd4d542b2517a2dcab19c447c")))
