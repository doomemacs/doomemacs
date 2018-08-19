;; -*- no-byte-compile: t; -*-
;;; feature/evil/packages.el

(package! evil)
(package! evil-args)
(package! evil-commentary)
(package! evil-easymotion)
(package! evil-embrace)
(package! evil-escape)
(package! evil-exchange)
(package! evil-indent-plus)
(package! evil-matchit)
(package! evil-numbers)
(package! evil-textobj-anyblock)
(package! evil-snipe)
(package! evil-surround)
(package! evil-vimish-fold)
(package! evil-visualstar)
(package! exato)


;;
(when (featurep! +everywhere)
  ;; `evil-collection-neotree' uses the `neotree-make-executor' macro, but this
  ;; requires neotree be available during byte-compilation (while installing).
  (when (featurep! :ui neotree)
    (depends-on! :ui neotree))

  (package! evil-collection))
