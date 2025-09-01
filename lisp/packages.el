;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode
  :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "cccd41f549fa88031a32deb26253b462021d7e12")
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
  :pin "25e02065419c8f26ad804f80463362862eb23edd")

;; doom-ui.el
(package! nerd-icons :pin "6868c05c6eb56c6625ee7fa38450b514542ab636")
(package! hide-mode-line :pin "ddd154f1e04d666cd004bf8212ead8684429350d")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "b1bf7a3c8cb820d942a0305e0e6412ef369f819c")
(package! dtrt-indent :pin "9108979357e8c9a1015baa5d37c0b596e2dda11b")
(package! smartparens :pin "b629b4e893ba21ba5a381f6c0054bb72f8e96df2")
(package! ws-butler
  ;; REVIEW: emacsmirror/nongnu_elpa serves this package from a branch. To stop
  ;;   Straight from clobbering a single repo for multiple packages, we must be
  ;;   explicit to force it to clone it multiple times.
  :recipe (:host github
           :repo "emacsmirror/nongnu_elpa"
           :branch "elpa/ws-butler"
           :local-repo "ws-butler")
  :pin "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a")

;; doom-projects.el
(package! projectile :pin "9325c45e0fd96d5421e75ad901a91ee5353e10ad")
(package! project :pin "a0ebb2e9cfda339d8ebda77447c9d822fe313c2d")

;; doom-keybinds.el
(package! general :pin "a48768f85a655fe77b5f45c2880b420da1b1b9c3")
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500")
