;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "509ca602c7aa565ab8f54c07e09af8fc3e706108")

(package! orderless :pin "ce462a63e32dd32bceea041f656bb79da953d62f")

(package! consult :pin "2106eee75f84206715631da45eae08827da266f9")
(package! consult-dir :pin "08f543ae6acbfc1ffe579ba1d00a5414012d5c0b")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "0ad7e8ff15683a4d64b79c29b3fcf847edfe244b"))

(package! embark :pin "56e28c23d56da3ae4b755bfa50a181bdedf439e6")
(package! embark-consult :pin "56e28c23d56da3ae4b755bfa50a181bdedf439e6")

(package! marginalia :pin "e9540a7b80f9c4d044748b88720e5cba3e30c20a")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion :pin "9e7d456b0934ecb568b6f05a8445e3f4ce32261f"))
