;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "ac9239bed5e3bfbf057382d1a75cdfa23f2caddd")
      (package! project :recipe (:host github :repo "emacs-straight/project")
        :pin "17a2f3260858115337f8bb08d5ea64457e4224bd"))
  (package! lsp-mode :pin "78df349a939f4f243591029f6ec8d4de6a71f5d0")
  (package! lsp-ui :pin "d92cf83d95c9ca177b735500ead88cf68dc2e2db")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "dce58b5509271bbedb53ba9d0278dcb563a43977"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "5018af9c709a783de1b9e101e07c948cceed67f1")))
