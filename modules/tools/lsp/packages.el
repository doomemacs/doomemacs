;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (package! eglot :pin "a5b7b7d933b97db9ce5f8b7dcc8c866f7c35b220")
  (package! lsp-mode :pin "aec8968364fce476f41e532bc083a96b6d9cb1ce")
  (package! lsp-ui :pin "cb02972b20706d18d137841c83d3765bcb280687")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "bccd86028e669f5a1cad78364775fe7a0741ff93"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92")))
