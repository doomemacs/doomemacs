;; -*- no-byte-compile: t; -*-
;;; editor/evil/packages.el

(package! evil :pin "3e41a823334abbba9cf16e482855699054d9dfe0")
(package! evil-args :pin "2671071a4a57eaee7cc8c27b9e4b6fc60fd2ccd3")
(package! evil-easymotion :pin "f96c2ed38ddc07908db7c3c11bcd6285a3e8c2e9")
(package! evil-embrace :pin "7b5a539cfe7db238d860122c793a0cb2d329cc6e")
(package! evil-escape
  :recipe (:host github :repo "hlissner/evil-escape")
  :pin "819f1ee1cf3f69a1ae920e6004f2c0baeebbe077")
(package! evil-exchange :pin "5f0a2d41434c17c6fb02e4f744043775de1c63a2")
(package! evil-indent-plus :pin "b4dacbfdb57f474f798bfbf5026d434d549eb65c")
(package! evil-lion :pin "4da660e124731ed65e7aaa6c067c30e876619429")
(package! evil-nerd-commenter :pin "42ba1a473b4f1df061baddd2f8b812a2f35e366e")
(package! evil-numbers :pin "08f0c1ee93b8a563770eaefaf21ab9087fca7bdb")
(package! evil-snipe :pin "a79177df406a79b4ffa25743c752f21363bba1cc")
(package! evil-surround :pin "282a975bda83310d20a2c536ac3cf95d2bf188a5")
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
(when (featurep! +everywhere)
  ;; `evil-collection-neotree' uses the `neotree-make-executor' macro, but this
  ;; requires neotree be available during byte-compilation (while installing).
  (when (featurep! :ui neotree)
    (package! neotree)
    (autoload 'neotree-make-executor "neotree" nil nil 'macro))

  (package! evil-collection :pin "e55718869252a8cd46e61e350bb514194a37f2f8"))
