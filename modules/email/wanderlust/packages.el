;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but they all have non-standard default
;;      branches which straight cannot detect without our help.
(package! apel :recipe (:branch "apel-wl") :pin "82eb2325bd149dc57b43a9ce9402c6c6183e4052")
(package! flim :recipe (:branch "flim-1_14-wl") :pin "abdd2315006eb31476249223569808adb1c0f7b2")
(package! semi :recipe (:branch "semi-1_14-wl") :pin "9063a4485b148a767ea924f0e7cc78d3524ba256")

(package! wanderlust :pin "9fd2c65e8d690625f35035a71e73f51f740dbe04")

(when (modulep! +xface)
  (package! x-face-e21
    :recipe (:host nil :repo "https://salsa.debian.org/debian/x-face-el.git"
             :files ("debian/x-face-e21.el"))
             :pin "871156a776cc1bc9dd035205b6875c55db6ae215"))
