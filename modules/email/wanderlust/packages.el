;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but they all have non-standard default
;;      branches which straight cannot detect without our help.
(package! apel :recipe (:branch "apel-wl") :pin "4e3269b6e702db2dba48cf560563ac883e81e3bf")
(package! flim :recipe (:branch "flim-1_14-wl") :pin "ddf5b6eceb73d7dbf6ff3a2d5281a2957cc2b836")
(package! semi :recipe (:branch "semi-1_14-wl") :pin "20d75302509b5fba9849e74b61c1ce93e5819864")

(package! wanderlust :pin "6e189fc944a9bbde76c5a6d9b6a38d57e85e6390")
