;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "9665359bb6bfb6a96b0c3b307d4abea9fcbff7a5")
      (when (featurep! :completion vertico)
        (package! consult-eglot :pin "f93c571dc392a8b11d35541bffde30bd9f411d30")))
  (package! lsp-mode :pin "237363939b2630a807261f31614ac98fad29b785")
  (package! lsp-ui :pin "d08c5528ba0a63433a466c2fa1265ec3250fcef1")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "b9aa9617f174a304040ae75d35483fa8d4ade5d7")))
