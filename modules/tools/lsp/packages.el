;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "9509b7e7e1f1e4214dc196bafcf56f42570fff9a")
      (package! jsonrpc :pin "70dc1806ae83abae7c6c9924a29d736d9393c279" :freeze t)
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "d8b444aac39edfc6473ffbd228df3e9119451b51"))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :pin "cd1dd78cec0ae1f566c765d98bbff322cc7b67ef"))
      (when (modulep! +booster)
        (package! eglot-booster
          :recipe (:host github :repo "jdtsmith/eglot-booster")
          :pin "cab7803c4f0adc7fff9da6680f90110674bb7a22")))

  ;; lsp-mode must be rebuilt if this variable is changed, so expose it here so
  ;; users can change it from $DOOMDIR/packages.el.
  (eval-and-compile (defvar lsp-use-plists t))

  (package! lsp-mode
    :pin "4c74da7ae51145f8e49c3544c90b410d96a742fa"
    :env `(("LSP_USE_PLISTS" . ,(and lsp-use-plists "1"))))
  (package! lsp-ui :pin "ff349658ed69086bd18c336c8a071ba15f7fd574")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "8e0b8fdec086375fd7560857a84cd78f5047ad9d"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "027897a957032e8752780600b21d2c0ea0cf3201"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "d11102c9db33c4ca7817296a2edafc3e26a61117")))
