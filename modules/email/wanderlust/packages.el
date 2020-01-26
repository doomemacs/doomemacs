;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but they all have non-standard default
;;      branches which straight cannot detect without our help.
(package! apel :recipe (:branch "apel-wl") :pin "d146ddbf88")
(package! flim :recipe (:branch "flim-1_14-wl") :pin "e4bd54fd7d")
(package! semi :recipe (:branch "semi-1_14-wl") :pin "16228dc2d1")

(package! wanderlust :pin "7a919e422a")
