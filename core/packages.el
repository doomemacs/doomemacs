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
  :pin "b45dd00408ff8e922f2d7f75441fd5603e5222fa")

;; core-modules.el
(package! use-package
  :type 'core
  :pin "a7422fb8ab1baee19adb2717b5b47b9c3812a84c")

;; core-ui.el
(package! all-the-icons :pin "9d97c074b08000eacefc03ebc5dadbdc33888cc2")
(package! hide-mode-line :pin "88888825b5b27b300683e662fa3be88d954b1cea")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "d576e6694ad3a3e88b2bb1363305b38fa364c149")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; core-editor.el
(package! better-jumper :pin "411ecdf6e7a3e1b4ced7605070d2309e5fc46556")
(package! dtrt-indent :pin "9714f2c5f1c9b7c21e732df8c15a870a88caba84")
(package! helpful :pin "7e4b1f0d5572a4e2b8ee7a9b084ef863d0315a73")
(package! pcre2el :pin "0b5b2a2c173aab3fd14aac6cf5e90ad3bf58fa7d")
(package! smartparens :pin "911cc896a0f2eb8b5fbdd6fc8331523ad9889a3a")
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler")
  :pin "2bb49d3ee7d2cba133bc7e9cdac416cd1c5e4fe0")

;; core-projects.el
(package! projectile :pin "da08a9103b5ba3b38ef031a9627a19436eb318b5")
(package! project :pin "4072f35d85bf0a1c669329d66633e4819f497c1c")

;; core-keybinds.el
(package! general :pin "a0b17d207badf462311b2eef7c065b884462cb7c")
(package! which-key :pin "27d9fec33abb989b030f7677ccf5f799287d6472")
