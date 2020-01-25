;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but both flim and apel have non-standard
;;      default branches, which straight cannot detect without our help.
(package! flim :recipe (:branch "flim-1_14-wl") :pin "e4bd54fd7d")
(package! apel :recipe (:branch "apel-wl") :pin "d146ddbf88")

(package! wanderlust :pin "7a919e422a")
