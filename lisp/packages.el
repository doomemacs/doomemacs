;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode
  :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "9a234d0d28cccd33f64faea6074fa2865a17c164")
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
  :pin "33fb4695066781c634ff1c3c81ba96e880deccf7")

;; doom-ui.el
(package! nerd-icons :pin "cc6c46830305df123de20b18510b15838e1608d6")
(package! hide-mode-line :pin "ddd154f1e04d666cd004bf8212ead8684429350d")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "f40ece58df8b2f0fb6c8576b527755a552a5e763")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "b1bf7a3c8cb820d942a0305e0e6412ef369f819c")
(package! dtrt-indent :pin "a8aa356684804c52f26602d4e315f1306c6f3e59")
(package! smartparens :pin "c7519a1b69f196050a13e2230b7532893b077086")
(package! ws-butler :pin "e3a38d93e01014cd47bf5af4924459bd145fd7c4")

;; doom-projects.el
(package! projectile :pin "0a15d81be953150399170f4b8e53b86ee96ad639")
(package! project :pin "2a3fe4c7dbb6cf4d449c4d358ec9aaf8658f6940")

;; doom-keybinds.el
(package! general :pin "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f")
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500")
