;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but they all have non-standard default
;;      branches which straight cannot detect without our help.
(package! apel :recipe (:branch "apel-wl") :pin "82eb2325bd149dc57b43a9ce9402c6c6183e4052")
(package! flim :recipe (:branch "flim-1_14-wl") :pin "2cf5a7891090faca8de725b1d3743dcedf233ea2")
(package! semi :recipe (:branch "semi-1_14-wl") :pin "7d8df0ef2f483e2dc063a72099295f467e6bc2c8")

(package! wanderlust :pin "77662986fd91cff991f502b19262227227740d52")
