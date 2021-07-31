;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "9de6709cddc09740d23d24fb425fa3c174d0e956")

(package! orderless :pin "1e84120a28525ccb47b602fc19b7afbeffbbe502")

(package! consult :pin "69bbd213dc8a98abe94a4f5b1920e3d689d31caa")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "92b259e6a8ebe6439f67d3d7ffa44b7e64b76478"))

(package! embark :pin "1a7e6b556142216fa5f9b897bd5eca73968f3c49")
(package! embark-consult :pin "1a7e6b556142216fa5f9b897bd5eca73968f3c49")

(package! marginalia :pin "11235445365c6ab119acabe91828e9182097ece7")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion
    :recipe (:host github :repo "iyefrat/all-the-icons-completion")
    :pin "d1d4b2f0dfbfa94d33fe50e8089c06601adfe674"))
