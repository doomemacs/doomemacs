;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "d2f34e5ce44dcf10c9bf2aab93278d1a8cb88655")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "d8b444aac39edfc6473ffbd228df3e9119451b51"))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :pin "0d7f0afc9bf08fce4a3ee225ec6540a91f8cfd76"))
      (when (modulep! +booster)
        (package! eglot-booster
          :recipe (:host github :repo "jdtsmith/eglot-booster")
          :pin "cab7803c4f0adc7fff9da6680f90110674bb7a22")))

  ;; HACK: Ensure lsp-mode is built with lsp-use-plists on, but also that
  ;;   `lsp-use-plists' isn't set *before* the package is rebuilt (which would
  ;;   break things).
  (defvar lsp-use-plists t)
  (add-hook 'straight-use-package-pre-build-functions
            (lambda (package)
              (when (equal package "lsp-mode")
                (let ((default-directory (doom-path doom-profile-dir doom-profile-init-dir-name))
                      (gen-file "01-modules-lsp-use-plists.el"))
                  (if (not lsp-use-plists)
                      (when (file-exists-p gen-file)
                        (delete-file gen-file))
                    (setenv "LSP_USE_PLISTS" "1")  ; ensure the setting propagates to child processes
                    (add-to-list 'doom-profile-generators
                                 (list gen-file (fn! `((setenv "LSP_USE_PLISTS" "1"))))))))))

  (package! lsp-mode :pin "c74a723870f86cf9d1b7aee5e6e2add10d9ce127")
  (package! lsp-ui :pin "030d36960338fd633a98b332bc3734c412c25ca6")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "2927cbc776477e23d4a1062568d55793eed33c51"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "95f21f3f672a3260806531878e7684cde23616b2"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "aef321d03907ca6926b0cf20ca85f672c4744000")))
