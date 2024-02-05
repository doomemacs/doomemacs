;; -*- no-byte-compile: t; -*-
;;; editor/evil/packages.el

(package! evil :pin "5995f6f21f662484440ed67a28ce59e365feb9ad")
(package! evil-args :pin "2671071a4a57eaee7cc8c27b9e4b6fc60fd2ccd3")
(package! evil-easymotion :pin "f96c2ed38ddc07908db7c3c11bcd6285a3e8c2e9")
(package! evil-embrace :pin "3081d37811b6a3dfaaf01d578c7ab7a746c6064d")
(package! evil-escape
  :recipe (:host github :repo "hlissner/evil-escape")
  :pin "819f1ee1cf3f69a1ae920e6004f2c0baeebbe077")
(package! evil-exchange :pin "5f0a2d41434c17c6fb02e4f744043775de1c63a2")
(package! evil-indent-plus :pin "f392696e4813f1d3a92c7eeed333248914ba6dae")
(package! evil-lion :pin "1e838a53b8f18a3c8bdf3e952186abc2ee9cb98e")
(package! evil-nerd-commenter :pin "3b197a2b559b06a7cf39978704b196f53dac802a")
(package! evil-numbers :pin "7a1b62afc12da2b582bf84d722e7b10ca8b97065")
(package! evil-snipe :pin "c2108d3932fcd2f75ac3e48250d6badd668f5b4f")
(package! evil-surround :pin "c7116cdc774b1e259eaf3e9e7a318a6c99c2da17")
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

  (package! evil-collection :pin "8c84f9bc89fe56e71b56519f886085ddcbc671cf"))
