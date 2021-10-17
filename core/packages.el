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
  :pin "f81d9b755fef36a8186d84dc071dd2f6011f89cf")

;; core-modules.el
(package! use-package
  :type 'core
  :pin "a7422fb8ab1baee19adb2717b5b47b9c3812a84c")

;; core-ui.el
(package! all-the-icons :pin "c0d288a41faea2ecb7e8dd947486764a2ee17ec9")
(package! hide-mode-line :pin "88888825b5b27b300683e662fa3be88d954b1cea")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "d576e6694ad3a3e88b2bb1363305b38fa364c149")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; core-editor.el
(package! better-jumper :pin "7f328a886ba4dd01993d269eee01c8ee3d0ddf52")
(package! dtrt-indent :pin "9714f2c5f1c9b7c21e732df8c15a870a88caba84")
(package! helpful :pin "7e4b1f0d5572a4e2b8ee7a9b084ef863d0315a73")
(package! pcre2el :pin "0b5b2a2c173aab3fd14aac6cf5e90ad3bf58fa7d")
(package! smartparens :pin "2834c66c4f09778d0c57e99886c329188eed591a")
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler")
  :pin "572a10c11b6cb88293de48acbb59a059d36f9ba5")

;; core-projects.el
(package! projectile :pin "7f64570d3e6829d767d340c8584f3e4f3472ee81")
(package! project :pin "a546cce47c434b0b4bf24bf1d5ed6e4623492072")

;; core-keybinds.el
(package! general :pin "26f1d4c4e258c40e6b70ce831a04800c914f29b3")
(package! which-key :pin "4790a14683a2f3e4f72ade197c78e4c0af1cdd4b")
