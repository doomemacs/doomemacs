;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "ac9239bed5e3bfbf057382d1a75cdfa23f2caddd")
      (package! project
        :recipe (:host github :repo "emacs-straight/project")
        :pin "da0333a697b18f0a863c1b1523d2fc7991b31174"))
  (package! lsp-mode :pin "5f3f9848b2d4afc69049121c60126a6405447106")
  (package! lsp-ui :pin "d92cf83d95c9ca177b735500ead88cf68dc2e2db")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "dce58b5509271bbedb53ba9d0278dcb563a43977"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "5018af9c709a783de1b9e101e07c948cceed67f1")))
