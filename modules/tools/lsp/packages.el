;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "e501275e06952889056268dabe08ccd0dbaf23e5")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "0da8801dd8435160ce1f62ad8066bd52e38f5cbd")))
  (package! lsp-mode :pin "a3b3c15359405f442fc51a2db09e503ca3b39f3d")
  (package! lsp-ui :pin "3cd7cc61273341023b863dcf45906ac9142fd1aa")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "9ecf4dd9b1207109802bd1882aa621eb1c385106"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "58b541476203fa68e9e7682531f2a10e11780857")))
