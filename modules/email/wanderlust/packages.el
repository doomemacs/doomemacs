;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but they all have non-standard default
;;      branches which straight cannot detect without our help.
(package! apel :recipe (:branch "apel-wl") :pin "1a6fd3bab2cc6b0a450c2d801f77a1c9da0f72fb")
(package! flim :recipe (:branch "flim-1_14-wl") :pin "23bb29d70a13cada2eaab425ef80071564586a6d")
(package! semi :recipe (:branch "semi-1_14-wl") :pin "85a52b899ac89be504d9e38d8d406bba98f4b0b3")

(package! wanderlust :pin "8b413b33cdb5a1b715f99a3919573fde2cfb3053")

(when (modulep! +xface)
  (package! x-face-e21
    :recipe (:host nil :repo "https://salsa.debian.org/debian/x-face-el.git"
             :files ("debian/x-face-e21.el"))
             :pin "871156a776cc1bc9dd035205b6875c55db6ae215"))
