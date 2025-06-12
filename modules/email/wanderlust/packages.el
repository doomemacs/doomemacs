;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but they all have non-standard default
;;      branches which straight cannot detect without our help.
(package! apel
  :recipe (:host github :repo "wanderlust/apel" :branch "apel-wl")
  :pin "2383abfd3a27c094ae9095b05103167cf810379b")
(package! flim
  :recipe (:host github :repo "wanderlust/flim" :branch "flim-1_14-wl")
  :pin "c4c72d28332083c909eb8a5719e1a7096bfac81f")
(package! semi
  :recipe (:host github :repo "wanderlust/semi" :branch "semi-1_14-wl")
  :pin "f6e299706ae33e7f4a775b94d52c8a2abe383a8a")

(package! wanderlust
  :recipe (:host github :repo "wanderlust/wanderlust")
  :pin "657eded150b7d16dc340b43e4863c7ea91564ed3")

(when (modulep! +xface)
  (package! x-face-e21
    :recipe (:host nil :repo "https://salsa.debian.org/debian/x-face-el.git"
             :files ("debian/x-face-e21.el"))
             :pin "871156a776cc1bc9dd035205b6875c55db6ae215"))
