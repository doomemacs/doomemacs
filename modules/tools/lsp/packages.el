;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "ac9239bed5e3bfbf057382d1a75cdfa23f2caddd")
      (package! project :pin "da0333a697b18f0a863c1b1523d2fc7991b31174"))
  (package! lsp-mode :pin "7b0b9c3785222ed89744a0d1376e764c30e950ac")
  (package! lsp-ui :pin "ce997c4dabb494ec4aaa93373ae27cd4d5cd0a4d")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "f6e321187e773d7e5dfb215802fff5f308226cf9"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "5018af9c709a783de1b9e101e07c948cceed67f1")))
