;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "fa8907c1e579b9376b58bfab1ace202f9c4f51b7")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "d8b444aac39edfc6473ffbd228df3e9119451b51"))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :pin "0d7f0afc9bf08fce4a3ee225ec6540a91f8cfd76"))
      (when (modulep! +booster)
        (package! eglot-booster
          :recipe (:host github :repo "jdtsmith/eglot-booster")
          :pin "cab7803c4f0adc7fff9da6680f90110674bb7a22")))

  ;; lsp-mode must be rebuilt if this variable is changed, so expose it here so
  ;; users can change it from $DOOMDIR/packages.el.
  (eval-and-compile (defvar lsp-use-plists t))

  (package! lsp-mode
    :pin "4c5360b451cc339c503f210eb8693fc2422c9666"
    :env `(("LSP_USE_PLISTS" . ,(and lsp-use-plists "1"))))
  (package! lsp-ui :pin "e5e38f3058bc6c3a108742ffa1048eebda6e5055")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "8e0b8fdec086375fd7560857a84cd78f5047ad9d"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "027897a957032e8752780600b21d2c0ea0cf3201"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "d11102c9db33c4ca7817296a2edafc3e26a61117")))
