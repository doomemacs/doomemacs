;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "0df75c0bbc545b1bd008718b1af2e6c0df18fe74")

(package! orderless :pin "62f71c34baca0b7d0adeab4a1c07d85ffcee80d9")

(package! consult :pin "dc6e45586194cb30b3ba7614189718f3db1391c3")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "92b259e6a8ebe6439f67d3d7ffa44b7e64b76478"))

(package! embark :pin "1492aefc00abc3355bf04c2ed05f40ff2f523fcf")
(package! embark-consult :pin "1492aefc00abc3355bf04c2ed05f40ff2f523fcf")

(package! marginalia :pin "cb1d3ba604dda17d8d44e7355ad76a1651830a30")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion :pin "96500418541b7376cd0b3e4583b9509c0dd92b27"))
