;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode
  :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "80dbd9bc5efee05a479663f8cfd0cc9e0a30dac5")
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
(package! dtrt-indent :pin "939c5e374ac0175bb7d561542e22e47a72d04aa8")
(package! helpful :pin "4ba24cac9fb14d5fdc32582cd947572040e82b2c")
(package! pcre2el :pin "380723b2701cceb75c266440fb8db918f3340d50")
(package! smartparens :pin "f7cf316715e5018186c226aab8242c9e5ce131c8")
(package! ws-butler :pin "e3a38d93e01014cd47bf5af4924459bd145fd7c4")

;; doom-projects.el
(package! projectile :pin "0163b335a18af0f077a474d4dc6b36e22b5e3274")
(package! project :pin "bf4c3cfcfbf3423d79170aa880a1abb332ed942e")

;; doom-keybinds.el
(package! general :pin "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f")
(package! which-key :pin "ed389312170df955aaf10c2e120cc533ed5c509e")
