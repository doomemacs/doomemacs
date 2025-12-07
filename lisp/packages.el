;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode
  :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "ad6a2b82f7bc2ea698b9684501963dfa71ffdc27")
(package! gcmh
  :pin "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9")

;; doom-packages.el
(package! straight
  :type 'core
  :recipe `(:host github
            :repo "radian-software/straight.el"
            :branch "develop"
            :local-repo "straight.el"
            :files ("straight*.el"))
  :pin "562639bd97cc3f62df24b168b8eb440bcd790650")

;; doom-ui.el
(package! nerd-icons :pin "772987a28d6408f840331c52c91d04b623a87048")
(package! hide-mode-line :pin "ddd154f1e04d666cd004bf8212ead8684429350d")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "b1bf7a3c8cb820d942a0305e0e6412ef369f819c")
(package! smartparens :pin "b629b4e893ba21ba5a381f6c0054bb72f8e96df2")

;; doom-projects.el
(package! projectile :pin "9325c45e0fd96d5421e75ad901a91ee5353e10ad")
(package! project :pin "83c91f5c8407b7ba8376fa966b1e573517a74e9d")

;; doom-keybinds.el
(package! general :pin "a48768f85a655fe77b5f45c2880b420da1b1b9c3")
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500")
