;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but they all have non-standard default
;;      branches which straight cannot detect without our help.
(package! apel :recipe (:branch "apel-wl") :pin "4e3269b6e702db2dba48cf560563ac883e81e3bf")
(package! flim :recipe (:branch "flim-1_14-wl") :pin "068b35e863be9245e717e79def55f93580188bc8")
(package! semi :recipe (:branch "semi-1_14-wl") :pin "f279ebe1c1f9c14bdd5d3da01af24277a6167b69")

(package! wanderlust :pin "5db307a0441b6b7aa4ecfd34344842d78f15611b")
