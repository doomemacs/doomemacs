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
  :pin "b3760f5829dba37e855add7323304561eb57a3d4")

;; doom-ui.el
(package! nerd-icons :pin "c6a4acf19454b415cba1c43daf4bfca8fccdd9ba")
(package! hide-mode-line :pin "bc5d293576c5e08c29e694078b96a5ed85631942")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "f40ece58df8b2f0fb6c8576b527755a552a5e763")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "47622213783ece37d5337dc28d33b530540fc319")
(package! dtrt-indent :pin "0230ec503283b895bd3df6c1e30b35a01aa0b9af")
(package! helpful :pin "a32a5b3d959a7fccf09a71d97b3d7c888ac31c69")
(package! pcre2el :pin "018531ba0cf8e2b28d1108136a0e031b6a45f1c1")
(package! smartparens :pin "0778a8a84064cf2bc3a9857bd0e7a4619cc1e5c3")
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler")
  :pin "572a10c11b6cb88293de48acbb59a059d36f9ba5")

;; doom-projects.el
(package! projectile :pin "e45f0b0cc43fdc066e7971ff3ed3bf4c78015ed0")
(package! project :pin "10a6b691e36ff897fb2a4b48896e08818afa77b0")

;; doom-keybinds.el
(package! general :pin "bda777cd303db217fd2fbf2087eff40ec4aafda1")
(package! which-key :pin "4d20bc852545a2e602f59084a630f888542052b1")

(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "eb8fbfa5582a8e5880e2eaa66d15d498bca6a45a")
