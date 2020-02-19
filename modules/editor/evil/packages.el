;; -*- no-byte-compile: t; -*-
;;; editor/evil/packages.el

(package! evil :pin "3b03a4ef9a")
(package! evil-args :pin "758ad5ae54")
(package! evil-easymotion :pin "79c13ed3bc")
(package! evil-embrace :pin "4379adea03")
(package! evil-escape :pin "f4e9116bfb")
(package! evil-exchange :pin "3030e21ee1")
(package! evil-indent-plus :pin "0c7501e6ef")
(package! evil-nerd-commenter :pin "c9fa23ee7e")
(package! evil-numbers
  :recipe (:host github :repo "janpath/evil-numbers")
  :pin "d988041c1f")
(package! evil-snipe :pin "3ec8adfd49")
(package! evil-surround :pin "9b0b17f06c")
(package! evil-textobj-anyblock :pin "ff00980f06")
(package! evil-traces :pin "bc25cae9fa")
(package! evil-visualstar :pin "06c053d8f7")
(package! exato :pin "88266fa7fc")
(package! evil-quick-diff
  :recipe (:host github :repo "rgrinberg/evil-quick-diff")
  :pin "69c883720b")

;;
(when (featurep! +everywhere)
  ;; `evil-collection-neotree' uses the `neotree-make-executor' macro, but this
  ;; requires neotree be available during byte-compilation (while installing).
  (when (featurep! :ui neotree)
    (package! neotree)
    (autoload 'neotree-make-executor "neotree" nil nil 'macro))

  (package! evil-collection :pin "e6a4ba695e"))
