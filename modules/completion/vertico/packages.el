;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "81a4b35f8d11dfad56de1727ee9bdd3b4461d07c")

(package! orderless :pin "62f71c34baca0b7d0adeab4a1c07d85ffcee80d9")

(package! consult :pin "a07ca383318cdce6935a370f1d17687ba9f225c3")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "92b259e6a8ebe6439f67d3d7ffa44b7e64b76478"))

(package! embark :pin "3a90a3e3c6cd035503d0c9de5c22875028e6da00")
(package! embark-consult :pin "3a90a3e3c6cd035503d0c9de5c22875028e6da00")

(package! marginalia :pin "cb1d3ba604dda17d8d44e7355ad76a1651830a30")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion :pin "96500418541b7376cd0b3e4583b9509c0dd92b27"))
