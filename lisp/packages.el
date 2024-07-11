;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode
  :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "09dce8a193c5a70277512263782b82fa1cba84c0")
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
  :pin "88e574ae75344e39b436f863ef0344135c7b6517")

;; doom-ui.el
(package! nerd-icons :pin "4322290303f2e12efd5685a0d22d76ed76ec7349")
(package! hide-mode-line :pin "bc5d293576c5e08c29e694078b96a5ed85631942")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "f40ece58df8b2f0fb6c8576b527755a552a5e763")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "47622213783ece37d5337dc28d33b530540fc319")
(package! dtrt-indent :pin "339755e4fb5245862737babf7f2c1e3bae1c129c")
(package! helpful :pin "4ba24cac9fb14d5fdc32582cd947572040e82b2c")
(package! smartparens :pin "ab475c78916d7b1666a495e3fe9c54b250195637")
(package! ws-butler :pin "e3a38d93e01014cd47bf5af4924459bd145fd7c4")

;; doom-projects.el
(package! projectile :pin "0163b335a18af0f077a474d4dc6b36e22b5e3274")
(package! project :pin "093f42a1b612eaae0d2bdd475663c14973fe0325")

;; doom-keybinds.el
(package! general :pin "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f")
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500")
