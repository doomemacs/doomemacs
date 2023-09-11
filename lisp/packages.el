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
(package! all-the-icons :pin "be9d5dcda9c892e8ca1535e288620eec075eb0be")
(package! nerd-icons :pin "619a0382d2e159f3142c4200fe4cfc2e89247ef1")
(package! hide-mode-line :pin "bc5d293576c5e08c29e694078b96a5ed85631942")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "f40ece58df8b2f0fb6c8576b527755a552a5e763")
(package! restart-emacs :pin "1607da2bc657fe05ae01f7fdf26f716eafead02c")

;; doom-editor.el
(package! better-jumper :pin "47622213783ece37d5337dc28d33b530540fc319")
(package! dtrt-indent :pin "e0630f74f915c6cded05f76f66d66e540fcc37c3")
(package! helpful :pin "66ba816b26b68dd7df08e86f8b96eaae16c8d6a2")
(package! pcre2el :pin "018531ba0cf8e2b28d1108136a0e031b6a45f1c1")
(package! smartparens :pin "79a338db115f441cd47bb91e6f75816c5e78a772")
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler")
  :pin "572a10c11b6cb88293de48acbb59a059d36f9ba5")

;; doom-projects.el
(package! projectile :pin "971cd5c4f25ff1f84ab7e8337ffc7f89f67a1b52")
(package! project :pin "ce140cdb70138a4938c999d4606a52dbeced4676")

;; doom-keybinds.el
(package! general :pin "833dea2c4a60e06fcd552b653dfc8960935c9fb4")
(package! which-key :pin "4d20bc852545a2e602f59084a630f888542052b1")

(package! compat
  :recipe (:host github :repo "emacs-compat/compat")
  :pin "ecf53005abf6f0325d14e0e024222e22e982c8dd")
