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
  :pin "392c29de0acdd5d309023c9f15df49eeb063a2fa")
(package! semi
  :recipe (:host github :repo "wanderlust/semi" :branch "semi-1_14-wl")
  :pin "5edbb0d925845a5c59abc03003569178a13d862f")

(package! wanderlust
  :recipe (:host github :repo "wanderlust/wanderlust")
  :pin "06ec9fa3979d6ff3f691bed0989e2a79fef71116")

(when (modulep! +xface)
  (package! x-face-e21
    :recipe (:host nil :repo "https://salsa.debian.org/debian/x-face-el.git"
             :files ("debian/x-face-e21.el"))
             :pin "871156a776cc1bc9dd035205b6875c55db6ae215"))
