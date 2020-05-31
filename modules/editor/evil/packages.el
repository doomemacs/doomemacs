;; -*- no-byte-compile: t; -*-
;;; editor/evil/packages.el

(package! evil :pin "25fc5c6647979357cf3e39f0667a9b7ae5266af9")
(package! evil-args :pin "758ad5ae54ad34202064fec192c88151c08cb387")
(package! evil-easymotion :pin "f96c2ed38ddc07908db7c3c11bcd6285a3e8c2e9")
(package! evil-embrace :pin "4379adea032b25e359d01a36301b4a5afdd0d1b7")
(package! evil-escape
  :recipe (:host github :repo "hlissner/evil-escape")
  :pin "819f1ee1cf3f69a1ae920e6004f2c0baeebbe077")
(package! evil-exchange :pin "3030e21ee16a42dfce7f7cf86147b778b3f5d8c1")
(package! evil-indent-plus :pin "0c7501e6efed661242c3a20e0a6c79a6455c2c40")
(package! evil-lion :pin "6b03593f5dd6e7c9ca02207f9a73615cf94c93ab")
(package! evil-nerd-commenter :pin "1bd2de52011c39777a3e8779b14cee2790dc873b")
(package! evil-numbers
  :recipe (:host github :repo "janpath/evil-numbers")
  :pin "c2cfdd1eb1f193bea28ee79b191b78309677058a")
(package! evil-snipe :pin "6dcac7f2516c6137a2de532fc2c052f242559ee3")
(package! evil-surround :pin "1c34944d8c98da4a2385d24ee89eef9cdf569a12")
(package! evil-textobj-anyblock :pin "ff00980f0634f95bf2ad9956b615a155ea8743be")
(package! evil-traces :pin "bc25cae9fa5ab0ba1507827f0944f52ce0ca7462")
(package! evil-visualstar :pin "06c053d8f7381f91c53311b1234872ca96ced752")
(package! exato :pin "aee7af7b7a0e7551478f453d1de7d5b9cb2e06c4")
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

  (package! evil-collection :pin "e065da3732f015428bd0068481dadad9e0e6d09c"))
