;; -*- no-byte-compile: t; -*-
;;; os/tty/packages.el

(if (modulep! +osc)
    (package! clipetty
      :recipe (:host github :repo "spudlyo/clipetty")
      :pin "01b39044b9b65fa4ea7d3166f8b1ffab6f740362")
  ;; Despite its name, this works for macOS as well.
  (package! xclip :pin "a1ac607f75a250dddf49866918bb493884451130"))

;; NOTE Despite the evil-* prefix, evil-terminal-cursor-changer does not depend
;;      on evil (anymore).
(package! evil-terminal-cursor-changer :pin "12ea9c0438c67e560b3866dc78b5c7d1d93f8cc5")
