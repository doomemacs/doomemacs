;; -*- no-byte-compile: t; -*-
;;; editor/evil/packages.el

(package! evil :pin "d14e9e2cac539f388a1f4699eb3ffab0834aa3e4")
(package! evil-args :pin "758ad5ae54ad34202064fec192c88151c08cb387")
(package! evil-easymotion :pin "79c13ed3bce018ac09d358e642e5bd7025e93603")
(package! evil-embrace :pin "4379adea032b25e359d01a36301b4a5afdd0d1b7")
(package! evil-escape :pin "f4e9116bfbaac8c9d210c17ad488e0982291245f")
(package! evil-exchange :pin "35dd0f0662789f043bd89a9f9801ffaf4318123f")
(package! evil-indent-plus :pin "0c7501e6efed661242c3a20e0a6c79a6455c2c40")
(package! evil-nerd-commenter :pin "a5555ff02a43ddc4b54ba38e19c5a233c3a0b304")
(package! evil-numbers
  :recipe (:host github :repo "janpath/evil-numbers")
  :pin "d988041c1fe6e941dc8d591390750b237f71f524")
(package! evil-snipe :pin "3ec8adfd4990f95fa0fab2b7019ead3596857673")
(package! evil-surround :pin "9b0b17f06cef9bac81ee4800d121265e54718a17")
(package! evil-textobj-anyblock :pin "ff00980f0634f95bf2ad9956b615a155ea8743be")
(package! evil-traces :pin "257c66bd7a9162caef3b04137af0dc9360fe3d53")
(package! evil-visualstar :pin "06c053d8f7381f91c53311b1234872ca96ced752")
(package! exato :pin "88266fa7fcfbef704032f671b94f756f2f98bd4f")
(package! evil-quick-diff
  :recipe (:host github :repo "rgrinberg/evil-quick-diff")
  :pin "69c883720b30a892c63bc89f49d4f0e8b8028908")

;;
(when (featurep! +everywhere)
  ;; `evil-collection-neotree' uses the `neotree-make-executor' macro, but this
  ;; requires neotree be available during byte-compilation (while installing).
  (when (featurep! :ui neotree)
    (package! neotree)
    (autoload 'neotree-make-executor "neotree" nil nil 'macro))

  (package! evil-collection :pin "8532282e6492ce92d8c54e43ce9e9ce616d8ab5f"))
