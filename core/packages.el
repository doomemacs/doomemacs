;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core.el
(package! auto-minor-mode :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! gcmh :pin "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9")
(package! explain-pause-mode
  :recipe (:host github
           :repo "lastquestion/explain-pause-mode")
  :pin "2356c8c3639cbeeb9751744dbe737267849b4b51")

;; core-packages.el
(package! straight
  :type 'core
  :recipe `(:host github
            :repo "raxod502/straight.el"
            :branch ,straight-repository-branch
            :local-repo "straight.el"
            :files ("straight*.el"))
  :pin "3277e1c9648b41dd5bfb239c067b8374ed2ec2bb")

;; core-modules.el
(package! use-package
  :type 'core
  :pin "caa92f1d64fc25480551757d854b4b49981dfa6b")

;; core-ui.el
(package! all-the-icons :pin "6917b08f64dd8487e23769433d6cb9ba11f4152f")
(package! hide-mode-line :pin "88888825b5b27b300683e662fa3be88d954b1cea")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "f43d48a24602be3ec899345a3326ed0247b960c6")
(package! restart-emacs :pin "e5707491d7ac20879465bb52e282ad1416748378")

;; core-editor.el
(package! better-jumper :pin "e3a6546aa626b9a79ae451c88f44cf26f9d1a919")
(package! dtrt-indent :pin "a7ade6d244eeeda2ada9f7eca565491cea4b622a")
(package! helpful :pin "584ecc887bb92133119f93a6716cdf7af0b51dca")
(package! pcre2el :pin "0b5b2a2c173aab3fd14aac6cf5e90ad3bf58fa7d")
(package! smartparens :pin "7f5825dd655942c3d56d14acabab1ffab1aa2ae2")
(package! so-long
  :built-in 'prefer ; included in Emacs 27+
  ;; REVIEW so-long is slated to be published to ELPA eventually, but until then
  ;;        I've created my own mirror for it because git.savannah.gnu.org runs
  ;;        on a potato.
  :recipe (:host github :repo "hlissner/emacs-so-long")
  :pin "ed666b0716f60e8988c455804de24b55919e71ca")
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler")
  :pin "2bb49d3ee7d2cba133bc7e9cdac416cd1c5e4fe0")

;; core-projects.el
(package! projectile :pin "d1cfad008b1719a6bee17fbe9479db414c0dc5d9")

;; core-keybinds.el
(package! general :pin "a0b17d207badf462311b2eef7c065b884462cb7c")
(package! which-key :pin "ca268fd313d3fb2bd03a8b5e4bdcca675ce58ca7")
