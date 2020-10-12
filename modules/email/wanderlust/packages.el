;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but they all have non-standard default
;;      branches which straight cannot detect without our help.
(package! apel :recipe (:branch "apel-wl") :pin "28bca5f7027da26c90bf25ab835a1d615ce316e4")
(package! flim :recipe (:branch "flim-1_14-wl") :pin "edb5982bdc24960798f6038db2c863d7c264cffb")
(package! semi :recipe (:branch "semi-1_14-wl") :pin "939c80580101126c81a72f1643762fbc964f8b64")

(package! wanderlust :pin "c7043e6446a302fee41b531d2daaa388c4d833a7")
