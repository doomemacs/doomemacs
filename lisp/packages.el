;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode
  :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "7179960b7c82a7cca6bac60d79dd7fe09ae390a0")
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
  :pin "d157afbec4ec8659ad2026f57354d58d849a7c94")

;; doom-ui.el
(package! nerd-icons :pin "14f7278dd7eb5eca762a6ff32467c72c661c0aae")
(package! hide-mode-line :pin "ddd154f1e04d666cd004bf8212ead8684429350d")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "f40ece58df8b2f0fb6c8576b527755a552a5e763")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "b1bf7a3c8cb820d942a0305e0e6412ef369f819c")
(package! dtrt-indent :pin "015b26d6d6af9465c1dc48ef721db119ecd78437")
(package! smartparens :pin "b0d935c11813bcd40f8d35bae8800e0741334c29")
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
(package! projectile :pin "4dd84b02c9cd7b04616dc2d01ba7bc87f0d15be8")
(package! project :pin "369ac661c8005349da1f79d3ed4fe72e5db7a9e4")

;; doom-keybinds.el
(package! general :pin "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f")
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500")
