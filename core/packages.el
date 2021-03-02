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
  :pin "0f9b828d8a41cf3d312678e82573066aebf2ab6e")

;; core-modules.el
(package! use-package
  :type 'core
  :pin "a7422fb8ab1baee19adb2717b5b47b9c3812a84c")

;; core-ui.el
(package! all-the-icons :pin "2f5ea7259ed104a0ef8727f640ee2525108038d5")
(package! hide-mode-line :pin "88888825b5b27b300683e662fa3be88d954b1cea")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "f43d48a24602be3ec899345a3326ed0247b960c6")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; core-editor.el
(package! better-jumper :pin "411ecdf6e7a3e1b4ced7605070d2309e5fc46556")
(package! dtrt-indent :pin "4a30d8edac7fbc5936fc07050e3ebfb94f97c1e7")
(package! helpful :pin "584ecc887bb92133119f93a6716cdf7af0b51dca")
(package! pcre2el :pin "0b5b2a2c173aab3fd14aac6cf5e90ad3bf58fa7d")
(package! smartparens :pin "63695c64233d215a92bf08e762f643cdb595bdd9")
;; DEPRECATED Built into Emacs 27+; remove when we drop 26 support
(package! so-long :built-in 'prefer :pin "a5d445de4829b2327bd51dad2fb04291c7a0ec5f")
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler")
  :pin "2bb49d3ee7d2cba133bc7e9cdac416cd1c5e4fe0")

;; core-projects.el
(package! projectile :pin "fd5994762a90c2311e8aa40c37373f24e1743a55")

;; core-keybinds.el
(package! general :pin "a0b17d207badf462311b2eef7c065b884462cb7c")
(package! which-key :pin "c0608e812a8d1bc7aefeacdfaeb56a7272eabf44")
