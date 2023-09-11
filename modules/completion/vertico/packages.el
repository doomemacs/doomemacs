;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "a28370d07f35c5387c7a9ec2e5b67f0d4598058d")

(package! orderless :pin "e6784026717a8a6a7dcd0bf31fd3414f148c542e")

(package! consult :pin "fe49dedd71802ff97be7b89f1ec4bd61b98c2b13")
(package! consult-dir :pin "ed8f0874d26f10f5c5b181ab9f2cf4107df8a0eb")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! consult-flycheck :pin "3f2a7c17cc2fe64e0c07e3bf90e33c885c0d7062"))
(package! embark :pin "9a44418c349e41020cdc5ad1bd21e8c77a429062")
(package! embark-consult :pin "9a44418c349e41020cdc5ad1bd21e8c77a429062")

(package! marginalia :pin "866e50aee4f066b0903752c69b33e9b7cab93f97")

(package! wgrep :pin "3132abd3750b8c87cbcf6942db952acfab5edccd")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "c2db8557a3c1a9588d111f8c8e91cae96ee85010"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "7da6d648ff4202a48eb6647ee7dce8d65de48779"))
