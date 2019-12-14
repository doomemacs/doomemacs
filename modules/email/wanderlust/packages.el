;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but both flim and apel have non-standard
;;      default branches, which straight cannot detect without our help.
(package! flim :recipe (:branch "flim-1_14-wl"))
(package! apel :recipe (:branch "semi-1_14-wl"))

(package! wanderlust)
