;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but they all have non-standard default
;;      branches which straight cannot detect without our help.
(package! apel
  :recipe (:host github :repo "wanderlust/apel" :branch "apel-wl")
  :pin "1a6fd3bab2cc6b0a450c2d801f77a1c9da0f72fb")
(package! flim
  :recipe (:host github :repo "wanderlust/flim" :branch "flim-1_14-wl")
  :pin "774e40da2b7de769e79c782dc82f09026a69163f")
(package! semi
  :recipe (:host github :repo "wanderlust/semi" :branch "semi-1_14-wl")
  :pin "85a52b899ac89be504d9e38d8d406bba98f4b0b3")

(package! wanderlust
  :recipe (:host github :repo "wanderlust/wanderlust")
  :pin "6a0605415d6661703e69fa2f860e179342b31e6f")

(when (modulep! +xface)
  (package! x-face-e21
    :recipe (:host nil :repo "https://salsa.debian.org/debian/x-face-el.git"
             :files ("debian/x-face-e21.el"))
             :pin "871156a776cc1bc9dd035205b6875c55db6ae215"))
