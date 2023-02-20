;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! gcmh :pin "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9")
(package! explain-pause-mode
  :recipe (:host github
           :repo "lastquestion/explain-pause-mode")
  :pin "2356c8c3639cbeeb9751744dbe737267849b4b51")

;; doom-packages.el
(package! straight
  :type 'core
  :recipe `(:host github
            :repo "radian-software/straight.el"
            :branch ,straight-repository-branch
            :local-repo "straight.el"
            :files ("straight*.el"))
  :pin "56a8b6c8a46fb78655d512402ad7840d85167225")

;; doom-ui.el
(package! all-the-icons :pin "51bf77da1ebc3c199dfc11f54c0dce67559f5f40")
(package! hide-mode-line :pin "bc5d293576c5e08c29e694078b96a5ed85631942")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "a32b39bdfe6c61c322c37226d66e1b6d4f107ed0")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "47622213783ece37d5337dc28d33b530540fc319")
(package! dtrt-indent :pin "d4fd1b4977eb0d534844fddf01c3c51c70c57205")
(package! helpful :pin "94c25337b2de2f9da60914a7c0c6cca9584c0231")
(package! pcre2el :pin "38c6f80c787da547287db96b495e5b695ca0b4b8")
(package! smartparens :pin "7afd647395018a26633673ed92ce7a9cb3ccb8f2")
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler")
  :pin "572a10c11b6cb88293de48acbb59a059d36f9ba5")

;; doom-projects.el
(package! projectile :pin "e18ad4d6111eb9e975ccce028baf5e4bb786bfcf")
(package! project :pin "e086bdc7c515f8bfc22598d473ba888ba7683f0c")

;; doom-keybinds.el
(package! general :pin "9651024e7f40a8ac5c3f31f8675d3ebe2b667344")
(package! which-key :pin "8093644032854b1cdf3245ce4e3c7b6673f741bf")

(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "38280a7b54a6220377835391ead8af6fa4839117")
