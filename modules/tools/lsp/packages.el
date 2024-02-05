;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "cd4e45b700b117d360aac24d749c3c4a4be3d2e2")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "049c6319b8a48ff66189d49592c7759f0b356596"))
      (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
        (package! flycheck-eglot :pin "9ff8d0068be59b1450964b390349d75a68af21ed")))
  (package! lsp-mode :pin "fb88cc6b8bcad4df5dd1d4e5d785adc7663e5c76")
  (package! lsp-ui :pin "bc58c6664577d1d79060c6b32b7ad20e70ee19d0")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "9ecf4dd9b1207109802bd1882aa621eb1c385106"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "f8db3252c0daa41225ba4ed1c0d178b281cd3e90")))
