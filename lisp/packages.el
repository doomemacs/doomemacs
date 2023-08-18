;; -*- no-byte-compile: t; -*-
;;; lisp/packages.el

;; doom.el
(package! auto-minor-mode :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! gcmh :pin "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9")
(package! explain-pause-mode
  :recipe (:host github
           :repo "lastquestion/explain-pause-mode")
  :pin "2356c8c3639cbeeb9751744dbe737267849b4b51")

;; doom-packages.el
(package! straight
  :type 'core
  :recipe `(:host github
            :repo "radian-software/straight.el"
            :branch ,straight-repository-branch
            :local-repo "straight.el"
            :files ("straight*.el"))
  :pin "5e84c4e2cd8ca79560477782ee4c9e5187725def")

;; doom-ui.el
(package! all-the-icons :pin "f491f39c21336d354e85bdb4cca281e0a0c2f880")
(package! hide-mode-line :pin "bc5d293576c5e08c29e694078b96a5ed85631942")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "a32b39bdfe6c61c322c37226d66e1b6d4f107ed0")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "47622213783ece37d5337dc28d33b530540fc319")
(package! dtrt-indent :pin "be07f4979a5b402a0cf5311c86c30b89ca0e1ee4")
(package! helpful :pin "c57ff0d284b50ff430fe1f13fd48deaa0d1a910e")
(package! pcre2el :pin "b941ed8a96299868171fac625ecffec77de3e986")
(package! smartparens :pin "79a338db115f441cd47bb91e6f75816c5e78a772")
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler")
  :pin "572a10c11b6cb88293de48acbb59a059d36f9ba5")

;; doom-projects.el
(package! projectile :pin "971cd5c4f25ff1f84ab7e8337ffc7f89f67a1b52")
(package! project :pin "6c41ad68edf1f44110abe478d17c36f57a517e66")

;; doom-keybinds.el
(package! general :pin "833dea2c4a60e06fcd552b653dfc8960935c9fb4")
(package! which-key :pin "df6b0cb8449812e7fb200bc852107fa7eb708496")

(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "75d0b8527f51aae42d23eee4aeb263e19055747e")
