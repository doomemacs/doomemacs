;; -*- no-byte-compile: t; -*-
;;; ui/smooth-scroll/packages.el

(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll")
  :pin "79151faf97c48005a8fac6a6883a4b9847cce1c8")

(when (modulep! +interpolate)
  (package! good-scroll :pin "a7ffd5c0e5935cebd545a0570f64949077f71ee3"))
