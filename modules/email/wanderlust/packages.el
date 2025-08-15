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
  :pin "56e3e0fd02168136fb954ac8273cc315621c3364")
(package! semi
  :recipe (:host github :repo "wanderlust/semi" :branch "semi-1_14-wl")
  :pin "813892fe2c18b88a71012d36a9c90b0c5f06e0c5")

(package! wanderlust
  :recipe (:host github :repo "wanderlust/wanderlust")
  :pin "70427e9c841d5253380e028a5bd01beac10574b9")

(when (modulep! +xface)
  (package! x-face-e21
    :recipe (:host nil :repo "https://salsa.debian.org/debian/x-face-el.git"
             :files ("debian/x-face-e21.el"))
             :pin "871156a776cc1bc9dd035205b6875c55db6ae215"))
