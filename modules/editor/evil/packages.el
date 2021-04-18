;; -*- no-byte-compile: t; -*-
;;; editor/evil/packages.el

(package! evil :pin "1b3db6349d04adfc68f707b0edd800f5ab3cb374")
(package! evil-args :pin "758ad5ae54ad34202064fec192c88151c08cb387")
(package! evil-easymotion :pin "f96c2ed38ddc07908db7c3c11bcd6285a3e8c2e9")
(package! evil-embrace :pin "464e8ec52ff78edf3c9060143fc375f6ce5f275f")
(package! evil-escape
  :recipe (:host github :repo "hlissner/evil-escape")
  :pin "819f1ee1cf3f69a1ae920e6004f2c0baeebbe077")
(package! evil-exchange :pin "ac50f21b29b6e3a111e10a9e88ae61c907ac5ee8")
(package! evil-indent-plus :pin "0c7501e6efed661242c3a20e0a6c79a6455c2c40")
(package! evil-lion :pin "6b03593f5dd6e7c9ca02207f9a73615cf94c93ab")
(package! evil-nerd-commenter :pin "b8ac35fe019df5602c31912f65303a3d8ad0066c")
(package! evil-numbers
  :recipe (:host github :repo "janpath/evil-numbers")
  :pin "006da406d175c05fedca4431cccd569e20bef92c")
(package! evil-snipe :pin "6dcac7f2516c6137a2de532fc2c052f242559ee3")
(package! evil-surround :pin "346d4d85fcf1f9517e9c4991c1efe68b4130f93a")
(package! evil-textobj-anyblock
  :recipe (:host github :repo "willghatch/evil-textobj-anyblock" :branch "fix-inner-block")
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

  (package! evil-collection :pin "b6025cf9eaf7b4659db918548fd6d96ed17fddc6"))
