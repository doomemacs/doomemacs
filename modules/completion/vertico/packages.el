;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "81a4b35f8d11dfad56de1727ee9bdd3b4461d07c")

(package! orderless :pin "1a7011ac9c476dbb083c5ead88462a5f520ef8aa")

(package! consult :pin "105a1ac50169382368a36ed53d7af908d02ffa07")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "92b259e6a8ebe6439f67d3d7ffa44b7e64b76478"))

(package! embark :pin "19145d5a3367038f0a9a114b7387c8a896037aec")
(package! embark-consult :pin "19145d5a3367038f0a9a114b7387c8a896037aec")

(package! marginalia :pin "c6ca58bea819c3b5e4b3295ad693c5cd0ae5db31")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion
    :recipe (:host github :repo "iyefrat/all-the-icons-completion")
    :pin "96500418541b7376cd0b3e4583b9509c0dd92b27"))
