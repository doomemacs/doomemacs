;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "e501275e06952889056268dabe08ccd0dbaf23e5")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "6504ccd2b7555452c61cc24a45965f7e2a37c44d"))
      (when (modulep! :checkers syntax)
        (package! flycheck-eglot :pin "9abab011071c93250a894cf2bfeaf30c8e3367a7")))
  (package! lsp-mode :pin "a655f3600e040f872408da0e9c1b9fe65ca0aad9")
  (package! lsp-ui :pin "295d8984da06a745b0a36c56e28ce915bc389adb")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "9ecf4dd9b1207109802bd1882aa621eb1c385106"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "f8db3252c0daa41225ba4ed1c0d178b281cd3e90")))
