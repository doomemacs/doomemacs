;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode :pin "87ea1df50f1659c521684f01bc518eac4bf7b2a0")
(package! lsp-ui :pin "242dfe859c3497c456eaacfd84942e12419529fe")
(when (featurep! :completion company)
  (package! company-lsp :pin "f921ffa0cdc542c21dc3dd85f2c93df4288e83bd"))
(when (featurep! :completion ivy)
  (package! lsp-ivy :pin "81e81ced99829358674c5a6bbe2c3e15cecd4ed8"))
(when (featurep! :completion helm)
  (package! helm-lsp :pin "6b5ce182d7c94c62b55b8f7d0c7e643b2c30e560"))
