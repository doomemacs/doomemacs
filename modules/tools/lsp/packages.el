;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "f110d26e0356983d07d94d38a1b384a0f79b6b8c")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "d8b444aac39edfc6473ffbd228df3e9119451b51"))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :pin "f617b68699f00e7fe271bbf6daaddde7e3b2fdb2"))
      (when (modulep! +booster)
        (package! eglot-booster
          :recipe (:host github :repo "jdtsmith/eglot-booster")
          :pin "cab7803c4f0adc7fff9da6680f90110674bb7a22")))

  ;; lsp-mode must be rebuilt if this variable is changed, so expose it here so
  ;; users can change it from $DOOMDIR/packages.el.
  (eval-and-compile (defvar lsp-use-plists t))

  (package! lsp-mode
    :pin "2315fdec79e5ce638a26a385d7744c57949f4560"
    :env `(("LSP_USE_PLISTS" . ,(and lsp-use-plists "1"))))
  (package! lsp-ui :pin "ff349658ed69086bd18c336c8a071ba15f7fd574")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "8e0b8fdec086375fd7560857a84cd78f5047ad9d"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "027897a957032e8752780600b21d2c0ea0cf3201"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "d11102c9db33c4ca7817296a2edafc3e26a61117")))
