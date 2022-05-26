;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but they all have non-standard default
;;      branches which straight cannot detect without our help.
(package! apel :recipe (:branch "apel-wl") :pin "6947dc4605ebbb87762edf7051a78a3f7b5f17c5")
(package! flim :recipe (:branch "flim-1_14-wl") :pin "289e5bbd66f6f14306a6e0b922ee8f26267e2470")
(package! semi :recipe (:branch "semi-1_14-wl") :pin "b1c245b81715b0430f7593cee2339e6264104f3d")

(package! wanderlust :pin "e3cd5e39454737c0b641e114ddcc550122288a2a")
