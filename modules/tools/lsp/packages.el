;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "a35ebe774d09421ea891593f8cb671b1656f8b86")
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
    :pin "c3b5fea5d6c3227801a69ea14080be29b6c3080e"
    :env `(("LSP_USE_PLISTS" . ,(and lsp-use-plists "1"))))
  (package! lsp-ui :pin "8547cd6abf3b474b3111911bc8ee1452afdfec8f")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "2927cbc776477e23d4a1062568d55793eed33c51"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "95f21f3f672a3260806531878e7684cde23616b2"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "d11102c9db33c4ca7817296a2edafc3e26a61117")))
