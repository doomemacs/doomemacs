;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode
  :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "8d4e8a366681def88751f5e9975738ecd3180deb")
(package! gcmh
  :pin "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9")

;; doom-packages.el
(package! straight
  :type 'core
  :recipe `(:host github
            :repo "radian-software/straight.el"
            :branch ,straight-repository-branch
            :local-repo "straight.el"
            :files ("straight*.el"))
  :pin "b1062df10ba4c10ff7a3c61b9e124b3242b11bb2")

;; doom-ui.el
(package! nerd-icons :pin "8095215a503d8048739de8b4ea4066598edb8cbb")
(package! hide-mode-line :pin "bc5d293576c5e08c29e694078b96a5ed85631942")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "f40ece58df8b2f0fb6c8576b527755a552a5e763")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "47622213783ece37d5337dc28d33b530540fc319")
(package! dtrt-indent :pin "5d1b44f9a1a484ca229cc14f8062609a10ef4891")
(package! helpful :pin "a32a5b3d959a7fccf09a71d97b3d7c888ac31c69")
(package! pcre2el :pin "380723b2701cceb75c266440fb8db918f3340d50")
(package! smartparens :pin "ddc6233ea6fc2da7a3a8e44face465c15631b02b")
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler")
  :pin "572a10c11b6cb88293de48acbb59a059d36f9ba5")

;; doom-projects.el
(package! projectile :pin "0163b335a18af0f077a474d4dc6b36e22b5e3274")
(package! project :pin "b6989856abe9411872bdff5c8aa190bef4d86409")

;; doom-keybinds.el
(package! general :pin "ced143c30de8e20f5a3761a465e684a1dc48471e")
(package! which-key :pin "96911a1d3faf8426a33241f4821319e98421f380")
