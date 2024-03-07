;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "24f2bf7b28c33e1d677b547956ade5560d27f55f")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "049c6319b8a48ff66189d49592c7759f0b356596"))
      (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
        (package! flycheck-eglot :pin "114e1315aaf0dc3196da67da426bbe2b46384fe2")))
  (package! lsp-mode :pin "a5f5ca9a8a4b2ceaf236457bf2524f94c183c2f2")
  (package! lsp-ui :pin "bc58c6664577d1d79060c6b32b7ad20e70ee19d0")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "9ecf4dd9b1207109802bd1882aa621eb1c385106"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "f8db3252c0daa41225ba4ed1c0d178b281cd3e90")))
