;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "76dae6a0756dd54946129a8ef170abcee63cf1ff")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "d8b444aac39edfc6473ffbd228df3e9119451b51"))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :pin "39dd981cc56c7cb8df510510bb8f3a17380980ee"))
      (when (modulep! +booster)
        (package! eglot-booster
          :recipe (:host github :repo "jdtsmith/eglot-booster")
          :pin "cab7803c4f0adc7fff9da6680f90110674bb7a22")))

  ;; lsp-mode must be rebuilt if this variable is changed, so expose it here so
  ;; users can change it from $DOOMDIR/packages.el.
  (eval-and-compile (defvar lsp-use-plists t))

  (package! lsp-mode
    :pin "c258b9dfaf83f2c9871069ab8ee85da7cf3bf042"
    :env `(("LSP_USE_PLISTS" . ,(and lsp-use-plists "1"))))
  (package! lsp-ui :pin "ff349658ed69086bd18c336c8a071ba15f7fd574")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "8e0b8fdec086375fd7560857a84cd78f5047ad9d"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "027897a957032e8752780600b21d2c0ea0cf3201"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "d11102c9db33c4ca7817296a2edafc3e26a61117")))
