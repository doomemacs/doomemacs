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
  :pin "92d4153362a87a585d16f31a2591324dac48e9f0")

;; core-modules.el
(package! use-package
  :type 'core
  :pin "a7422fb8ab1baee19adb2717b5b47b9c3812a84c")

;; core-ui.el
(package! all-the-icons :pin "483dba65e897071c156cefec937edcf51aa333db")
(package! hide-mode-line :pin "bc5d293576c5e08c29e694078b96a5ed85631942")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "d576e6694ad3a3e88b2bb1363305b38fa364c149")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; core-editor.el
(package! better-jumper :pin "3148a17b5920bba8ec4f81b717b99acde5fd5b74")
(package! dtrt-indent :pin "1986ad4e60f2e21f69d77ef9fb14da80a6157866")
(package! helpful :pin "2afbde902742b1aa64daa31a635ba564f14b35ae")
(package! pcre2el :pin "0b5b2a2c173aab3fd14aac6cf5e90ad3bf58fa7d")
(package! smartparens :pin "f59a40d54f35299007c396bd667ce3e9ec4714e3")
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler")
  :pin "572a10c11b6cb88293de48acbb59a059d36f9ba5")

;; core-projects.el
(package! projectile :pin "2bb7ec28b1275bbce7cac743ee9e7b2cf41c5bbd")
(package! project :pin "401e4aa68e5f46fe72ca6f9a1cb07cd84f5ee981")

;; core-keybinds.el
(package! general :pin "9651024e7f40a8ac5c3f31f8675d3ebe2b667344")
(package! which-key :pin "1bb1f723dab2fc8b88b7f7273d0a7fa11134b936")
