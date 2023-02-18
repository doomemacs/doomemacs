;; -*- no-byte-compile: t; -*-
;;; editor/evil/packages.el

(package! evil :pin "2ce03d412c4e93b0b89eb43d796c991806415b8a")
(package! evil-args :pin "2671071a4a57eaee7cc8c27b9e4b6fc60fd2ccd3")
(package! evil-easymotion :pin "f96c2ed38ddc07908db7c3c11bcd6285a3e8c2e9")
(package! evil-embrace :pin "7b5a539cfe7db238d860122c793a0cb2d329cc6e")
(package! evil-escape
  :recipe (:host github :repo "hlissner/evil-escape")
  :pin "819f1ee1cf3f69a1ae920e6004f2c0baeebbe077")
(package! evil-exchange :pin "5f0a2d41434c17c6fb02e4f744043775de1c63a2")
(package! evil-indent-plus :pin "b4dacbfdb57f474f798bfbf5026d434d549eb65c")
(package! evil-lion :pin "a55eb647422342f6b1cf867f797b060b3645d9d8")
(package! evil-nerd-commenter :pin "8c0f23d46a3927b9f83c1c2c4590be53d0b740db")
(package! evil-numbers :pin "7a1b62afc12da2b582bf84d722e7b10ca8b97065")
(package! evil-snipe :pin "c07788c35cf8cd8e652a494322fdc0643e30a89f")
(package! evil-surround :pin "f273821f575ace519066fb106ee45a5b8577475f")
(package! evil-textobj-anyblock
  :recipe (:host github
           :repo "willghatch/evil-textobj-anyblock"
           :branch "fix-inner-block")
  :pin "29280cd71a05429364cdceef2ff595ae8afade4d")
(package! evil-traces :pin "290b5323542c46af364ec485c8ec9000040acf90")
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

  (package! evil-collection :pin "aaf3e0038e9255659fe0455729239c08498c4c0b"))
