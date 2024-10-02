;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode
  :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "e9203e164903a6bb7de3e58aa0d653bbcff9d3d1")
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
  :pin "08e197bfa63829f5f5ce42cf1e3c4a4e40003f5a")

;; doom-ui.el
(package! nerd-icons :pin "c3d641d8e14bd11b5f98372da34ee5313636e363")
(package! hide-mode-line :pin "ddd154f1e04d666cd004bf8212ead8684429350d")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "f40ece58df8b2f0fb6c8576b527755a552a5e763")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "47622213783ece37d5337dc28d33b530540fc319")
(package! dtrt-indent :pin "a8aa356684804c52f26602d4e315f1306c6f3e59")
(package! smartparens :pin "c7519a1b69f196050a13e2230b7532893b077086")
(package! ws-butler :pin "e3a38d93e01014cd47bf5af4924459bd145fd7c4")

;; doom-projects.el
(package! projectile :pin "8cc2ee8937b89f1639304cbd2526e85b17135372")
(package! project :pin "5c77d78936364e2e6e9641af2091fde0bee729ce")

;; doom-keybinds.el
(package! general :pin "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f")
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500")
