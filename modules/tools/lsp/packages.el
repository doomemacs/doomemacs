;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "2b87b06d9ef15e7c39d87fd5a4375b6deaa7e322")
      (when (featurep! :completion vertico)
        (package! consult-eglot :pin "0da8801dd8435160ce1f62ad8066bd52e38f5cbd")))
  (package! lsp-mode :pin "9faa49269234c3ad39d66e6baba6642bf2653f86")
  (package! lsp-ui :pin "370022b6785e5c1906cf23fe92d658f10623f3ba")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "19606a03cf854e1b0930c4526ed92c4560dccdc2")))
