;; -*- no-byte-compile: t; -*-
;;; feature/evil/packages.el

;; `evil-collection' uses the `with-helm-buffer' macro, but this requires helm
;; be loaded before it is byte-compiled during installation. To ensure this, we
;; declare helm before evil-collection.
(when (featurep! :completion helm)
  (depends-on! :completion helm))

;;
(package! evil)
(package! evil-args)
(package! evil-commentary)
(package! evil-collection)
(package! evil-easymotion)
(package! evil-ediff)
(package! evil-embrace)
(package! evil-escape)
(package! evil-exchange)
(package! evil-indent-plus)
(package! evil-matchit)
(package! evil-mc)
(package! evil-multiedit)
(package! evil-numbers)
(package! evil-textobj-anyblock)
(package! evil-snipe)
(package! evil-surround)
(package! evil-vimish-fold)
(package! evil-visualstar)
(package! exato)
