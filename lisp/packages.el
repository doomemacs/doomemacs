;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode
  :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "38df650dce9f862c6b523de5b683573df590ab85")
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
  :pin "09b789a8596cacca6bbff866500373541a85ffa2")

;; doom-ui.el
(package! nerd-icons :pin "9a7f44db9a53567f04603bc88d05402cad49c64c")
(package! hide-mode-line :pin "ddd154f1e04d666cd004bf8212ead8684429350d")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "b1bf7a3c8cb820d942a0305e0e6412ef369f819c")
(package! smartparens :pin "82d2cf084a19b0c2c3812e0550721f8a61996056")

;; doom-projects.el
(package! projectile :pin "7cb1f64c1d68dd01acda8b7e3459ed11eb4fb2a8")
(package! project :pin "ff700457fee13ae82f00b7522e4fe5fa333e3533")

;; doom-keybinds.el
(package! general :pin "a48768f85a655fe77b5f45c2880b420da1b1b9c3")
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500")
