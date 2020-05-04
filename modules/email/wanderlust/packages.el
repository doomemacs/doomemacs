;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but they all have non-standard default
;;      branches which straight cannot detect without our help.
(package! apel :recipe (:branch "apel-wl") :pin "d146ddbf8818e81d3577d5eee7825d377bec0c73")
(package! flim :recipe (:branch "flim-1_14-wl") :pin "f303f2f6c124bc8635add96d3326a2209749437b")
(package! semi :recipe (:branch "semi-1_14-wl") :pin "57a948c5f07e57e78ab3c0e6fd76ffcd591bb4ac")

(package! wanderlust :pin "7af0d582cd48a37469e0606ea35887740d78c8b5")
