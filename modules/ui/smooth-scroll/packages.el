;; -*- no-byte-compile: t; -*-
;;; ui/smooth-scroll/packages.el

(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll")
  :pin "d230b9308c5891abf8378b1030af9e1b029e9b3b")

(when (modulep! +interpolate)
  (package! good-scroll :pin "a7ffd5c0e5935cebd545a0570f64949077f71ee3"))
