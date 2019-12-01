;; -*- no-byte-compile: t; -*-
;;; editor/evil/packages.el

(package! evil)
(package! evil-args)
(package! evil-easymotion)
(package! evil-embrace)
(package! evil-escape)
(package! evil-exchange)
(package! evil-indent-plus)
(package! evil-nerd-commenter)
(package! evil-numbers :recipe (:host github :repo "janpath/evil-numbers"))
(package! evil-snipe)
(package! evil-surround)
(package! evil-textobj-anyblock)
(package! evil-traces)
(package! evil-visualstar)
(package! exato)
(package! evil-quick-diff :recipe (:host github :repo "rgrinberg/evil-quick-diff"))

;;
(when (featurep! +everywhere)
  ;; `evil-collection-neotree' uses the `neotree-make-executor' macro, but this
  ;; requires neotree be available during byte-compilation (while installing).
  (when (featurep! :ui neotree)
    (package! neotree)
    (autoload 'neotree-make-executor "neotree" nil nil 'macro))

  (package! evil-collection))
