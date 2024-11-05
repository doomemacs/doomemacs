;; -*- no-byte-compile: t; -*-
;;; os/tty/packages.el

(if (modulep! +osc)
    (package! clipetty
      :recipe (:host github :repo "spudlyo/clipetty")
      :pin "01b39044b9b65fa4ea7d3166f8b1ffab6f740362")
  ;; Despite its name, this works for macOS as well.
  (package! xclip :pin "9ab22517f3f2044e1c8c19be263da9803fbca26a"))

;; NOTE Despite the evil-* prefix, evil-terminal-cursor-changer does not depend
;;      on evil (anymore).
(package! evil-terminal-cursor-changer :pin "2358f3e27d89128361cf80fcfa092fdfe5b52fd8")

(package! kkp :pin "ed9214329f11b095fc7bad06feb329b9f232258d")
