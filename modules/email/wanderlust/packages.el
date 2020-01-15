;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but both flim and apel have non-standard
;;      default branches, which straight cannot detect without our help.
(package! flim :recipe (:branch "flim-1_14-wl")
  :pin "e4bd54fd7d335215b54f7ef27ed974c8cd68d472")
(package! apel :recipe (:branch "semi-1_14-wl")
  :pin "d146ddbf8818e81d3577d5eee7825d377bec0c73")

(package! wanderlust :pin "7a919e422a48f5021576e68282703de430558879")
