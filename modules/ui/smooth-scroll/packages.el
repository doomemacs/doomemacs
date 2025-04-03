;; -*- no-byte-compile: t; -*-
;;; ui/smooth-scroll/packages.el

(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll")
  :pin "b72c507f6702db18d971a6b6bdc692e260f21159")

(when (modulep! +interpolate)
  (package! good-scroll :pin "a7ffd5c0e5935cebd545a0570f64949077f71ee3"))
