;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode
  :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "2577cc74d996620766adf1c9ec8f44ecbac32e79")
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
  :pin "4128d423998696d03cca2acb6b8828e907e8e0d9")

;; doom-ui.el
(package! nerd-icons :pin "d972dee349395ffae8fceae790d22fedc8fe08e8")
(package! hide-mode-line :pin "ddd154f1e04d666cd004bf8212ead8684429350d")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "b1bf7a3c8cb820d942a0305e0e6412ef369f819c")
(package! dtrt-indent :pin "015b26d6d6af9465c1dc48ef721db119ecd78437")
(package! smartparens :pin "b629b4e893ba21ba5a381f6c0054bb72f8e96df2")
(package! ws-butler
  ;; REVIEW: emacsmirror/nongnu_elpa serves this package from a branch. To stop
  ;;   Straight from clobbering a single repo for multiple packages, we must be
  ;;   explicit to force it to clone it multiple times.
  :recipe (:host github
           :repo "emacsmirror/nongnu_elpa"
           :branch "elpa/ws-butler"
           :local-repo "ws-butler")
  :pin "9ee5a7657a22e836618813c2e2b64a548d27d2ff")

;; doom-projects.el
(package! projectile :pin "0da59734fbc23fc26222a7d03f6671b3116b0b77")
(package! project :pin "de0deb8dbfb65eae6eaaf45aea77f12aa9a25f89")

;; doom-keybinds.el
(package! general :pin "74d4541781d3a01933dda298eb932c06e67413f9")
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500")
