;; -*- no-byte-compile: t; -*-
;;; editor/evil/packages.el

(package! evil :pin "334a636621577e77f834bca0c6ecdcec67c6ff1e")
(package! evil-args :pin "a8151556f63c9d45d0c44c8a7ef9e5a542f3cdc7")
(package! evil-easymotion :pin "f96c2ed38ddc07908db7c3c11bcd6285a3e8c2e9")
(package! evil-embrace :pin "3081d37811b6a3dfaaf01d578c7ab7a746c6064d")
(package! evil-escape
  :recipe (:host github :repo "hlissner/evil-escape")
  :pin "819f1ee1cf3f69a1ae920e6004f2c0baeebbe077")
(package! evil-exchange :pin "5f0a2d41434c17c6fb02e4f744043775de1c63a2")
(package! evil-indent-plus :pin "f392696e4813f1d3a92c7eeed333248914ba6dae")
(package! evil-lion :pin "5a0bca151466960e090d1803c4c5ded88875f90a")
(package! evil-nerd-commenter :pin "ae52c5070a48793e2c24474c9c8dbf20175d18a0")
(package! evil-numbers :pin "e96d656158e3c712a33cced2ea1a088f44448fe3")
(package! evil-snipe :pin "16317d7e54313490a0fe8642ed9a1a72498e7ad2")
(package! evil-surround :pin "da05c60b0621cf33161bb4335153f75ff5c29d91")
(package! evil-textobj-anyblock
  :recipe (:host github
           :repo "willghatch/evil-textobj-anyblock"
           :branch "fix-inner-block")
  :pin "29280cd71a05429364cdceef2ff595ae8afade4d")
(package! evil-traces :pin "82e8a7b4213aed140f6eb5f2cc33a09bb5587166")
(package! evil-visualstar :pin "06c053d8f7381f91c53311b1234872ca96ced752")
(package! exato :pin "aee7af7b7a0e7551478f453d1de7d5b9cb2e06c4")
(package! evil-quick-diff
  :recipe (:host github :repo "rgrinberg/evil-quick-diff")
  :pin "69c883720b30a892c63bc89f49d4f0e8b8028908")

;;
(when (modulep! +everywhere)
  ;; `evil-collection-neotree' uses the `neotree-make-executor' macro, but this
  ;; requires neotree be available during byte-compilation (while installing).
  (when (modulep! :ui neotree)
    (package! neotree)
    (autoload 'neotree-make-executor "neotree" nil nil 'macro))

  (package! evil-collection :pin "faed16f485bf15c95f4544fbb249b5f07c0007fc"))
