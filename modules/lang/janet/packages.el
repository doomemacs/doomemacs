;; -*- no-byte-compile: t; -*-
;;; lang/janet/packages.el

(package! janet-mode :pin "9e3254a0249d720d5fa5603f1f8c3ed0612695af")
(package! inf-janet
  :recipe (:host github :repo "velkyel/inf-janet")
  :pin "7d1d350890f67d3ece63b219985231d1c02368fb")
