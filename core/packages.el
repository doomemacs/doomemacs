;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core.el
(package! auto-minor-mode :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! gcmh :pin "b1bde5089169a74f62033d027e06e98cbeedd43f")
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
            :files ("straight*.el")
            :no-build t)
  :pin "0c7c7571349b628d87acde474a754f05e86ca876")

;; core-modules.el
(package! use-package
  :type 'core
  :pin "4fb1f9a68f1e7e7d614652afc017a6652fd029f1")

;; core-ui.el
(package! all-the-icons :pin "8c0228053dd6693d926970d89270094be52b0f75")
(package! hide-mode-line :pin "88888825b5b27b300683e662fa3be88d954b1cea")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "5125f4e47604ad36c3eb4706310fcafac729ca8c")
(package! restart-emacs :pin "9aa90d3df9e08bc420e1c9845ee3ff568e911bd9")

;; core-editor.el
(package! better-jumper :pin "fe548d22c9228b60d9c8a2a452a6c2e03dfdf238")
(package! dtrt-indent :pin "50c440c80e0d15303d8ab543bce4c56e9c2bf407")
(package! helpful :pin "c0662aa07266fe204f4e6d72ccaa6af089400556")
(package! pcre2el :pin "0b5b2a2c173aab3fd14aac6cf5e90ad3bf58fa7d")
(package! smartparens :pin "555626a43f9bb1985aa9a0eb675f2b88b29702c8")
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
(package! projectile :pin "46d2010c6a6cccfc4be72317f10ea99fd041ab54")

;; core-keybinds.el
(package! general :pin "a0b17d207badf462311b2eef7c065b884462cb7c")
(package! which-key :pin "e48e190a75a0c176e1deac218b891e77792d6921")
