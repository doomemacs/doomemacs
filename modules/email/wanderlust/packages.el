;; -*- no-byte-compile: t; -*-
;;; app/wanderlust/packages.el

;; HACK These are wanderlust's dependencies (wanderlust depends on semi, semi
;;      depends on flim, flim on apel), but they all have non-standard default
;;      branches which straight cannot detect without our help.
(package! apel :recipe (:branch "apel-wl") :pin "82eb2325bd149dc57b43a9ce9402c6c6183e4052")
(package! flim :recipe (:branch "flim-1_14-wl") :pin "80b8121f05a5a0d7fcfe3e54085467a646dd2028")
(package! semi :recipe (:branch "semi-1_14-wl") :pin "9370961ddcee78e389e44b36d38c3d93f8351619")

(package! wanderlust :pin "8369b2d5170a174652294835dd9a18ed21a38cb2")
