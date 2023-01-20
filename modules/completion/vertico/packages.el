;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "bedd146c3ffc236d746d088a94c3858eca0618d9")

(package! orderless :pin "847694e78c12d903d5e3f6cb365a5d3b984db537")

(package! consult :pin "16b2dc5e34c8a500adbee394b42c0e0d7fd24ad8")
(package! compat :pin "7ca7d300d1d256f674f83932d2918d8e70cd28f6")
(package! consult-dir :pin "ed8f0874d26f10f5c5b181ab9f2cf4107df8a0eb")
(when (modulep! :checkers syntax)
  (package! consult-flycheck :pin "7a10be316d728d3384fa25574a30857c53fb3655"))

(package! embark :pin "629cce948c562361ddd6136d7cc49c5c981bb610")
(package! embark-consult :pin "629cce948c562361ddd6136d7cc49c5c981bb610")

(package! marginalia :pin "c1365bf0c7b5d32e7531fa8f1a9a3b64a155cec0")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (modulep! +icons)
  (package! all-the-icons-completion :pin "4da28584a1b36b222e0e78d46fd8d46bbd9116c7"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "a3d0802d7b4a64be1c8c9344fe2de99f2c5ce7ff"))
