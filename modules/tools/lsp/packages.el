;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode :pin "e95109b8b84201c299f54d5f22e84e5eb6511ac4")
(package! lsp-ui :pin "01f89e40f473032376f70a90e33831356832f084")
(when (featurep! :completion company)
  (package! company-lsp :pin "f921ffa0cdc542c21dc3dd85f2c93df4288e83bd"))
(when (featurep! :completion ivy)
  (package! lsp-ivy :pin "78c1429c62c19006058b89d462657e1448d1e595"))
(when (featurep! :completion helm)
  (package! helm-lsp :pin "6f62659cc528b7e37ffcc8fb356633acd7031be8"))
