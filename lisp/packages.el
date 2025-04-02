;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode
  :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "c89bba6524501bde03db6266a3ac47b266b81e02")
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
  :pin "483b205efb2eaa6be7c0dc7078b8c9dafcffb318")

;; doom-ui.el
(package! nerd-icons :pin "6612cc65373b63e85362b6a5d0bbd440b05be58b")
(package! hide-mode-line :pin "ddd154f1e04d666cd004bf8212ead8684429350d")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "f40ece58df8b2f0fb6c8576b527755a552a5e763")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "b1bf7a3c8cb820d942a0305e0e6412ef369f819c")
(package! dtrt-indent :pin "22498ca24ac93c051d233abef630aece1ac45dd1")
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
(package! projectile :pin "55db082cdf7b849335ccf24b7ba5aa2607d6fe93")
(package! project :pin "d193442fca61b41803baf6446e4052ed034869a6")

;; doom-keybinds.el
(package! general :pin "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f")
(package! which-key :pin "38d4308d1143b61e4004b6e7a940686784e51500")
