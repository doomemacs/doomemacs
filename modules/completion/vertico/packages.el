;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "a8fe9a0b2e156e022136169a3159b4dad78b2439")

(package! orderless :pin "1ccf74ffdbb0dd34caa63022e92f947c09c49c86")

(package! consult :pin "0940ca016531f3412003c231b476e5023a510ff9")
(package! consult-dir :pin "08f543ae6acbfc1ffe579ba1d00a5414012d5c0b")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "0ad7e8ff15683a4d64b79c29b3fcf847edfe244b"))

(package! embark :pin "c9b26c2e18f01ae401df6a69b7a0c1a6bc44b90c")
(package! embark-consult :pin "c9b26c2e18f01ae401df6a69b7a0c1a6bc44b90c")

(package! marginalia :pin "9229d88ae4757f3439e81f51799758c009838cb4")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion :pin "9e7d456b0934ecb568b6f05a8445e3f4ce32261f"))
