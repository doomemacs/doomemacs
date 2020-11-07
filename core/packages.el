;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core.el
(package! auto-minor-mode :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! gcmh :pin "84c43a4c0b41a595ac6e299fa317d2831813e580")
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
  :pin "0f283e2f92c106d5bbb558862d433954fc8db179")

;; core-modules.el
(package! use-package
  :type 'core
  :pin "4fb1f9a68f1e7e7d614652afc017a6652fd029f1")

;; core-ui.el
(package! all-the-icons :pin "6917b08f64dd8487e23769433d6cb9ba11f4152f")
(package! hide-mode-line :pin "88888825b5b27b300683e662fa3be88d954b1cea")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "f43d48a24602be3ec899345a3326ed0247b960c6")
(package! restart-emacs :pin "9aa90d3df9e08bc420e1c9845ee3ff568e911bd9")

;; core-editor.el
(package! better-jumper :pin "fe548d22c9228b60d9c8a2a452a6c2e03dfdf238")
(package! dtrt-indent :pin "50c440c80e0d15303d8ab543bce4c56e9c2bf407")
(package! helpful :pin "584ecc887bb92133119f93a6716cdf7af0b51dca")
(package! pcre2el :pin "0b5b2a2c173aab3fd14aac6cf5e90ad3bf58fa7d")
(package! smartparens :pin "c59bfef7e8f1687ac77b0afaaaed86d8051d3de1")
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
(package! projectile :pin "d1daf274e8ca2eb0f20475b8f314bb955167c6a1")

;; core-keybinds.el
(package! general :pin "a0b17d207badf462311b2eef7c065b884462cb7c")
(package! which-key :pin "c011b268196b8356c70f668506a1133086bc9477")
