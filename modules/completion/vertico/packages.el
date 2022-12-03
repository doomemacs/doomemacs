;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "801ad3143d26653384f4c25bad44f7c098dd704c")

(package! orderless :pin "004cee6b8e01f8eb0cb1c683d0a637b14890600f")

(package! consult :pin "e4e2af1a2d06d40461d975b74ea3cc863cd18085")
(package! compat :pin "056e3ccffc716990dcb7b33273453d5fce0402de")
(package! consult-dir :pin "ed8f0874d26f10f5c5b181ab9f2cf4107df8a0eb")
(when (modulep! :checkers syntax)
  (package! consult-flycheck :pin "7a10be316d728d3384fa25574a30857c53fb3655"))

(package! embark :pin "09da327d43793f0b30114ee80d82ef587124462a")
(package! embark-consult :pin "09da327d43793f0b30114ee80d82ef587124462a")

(package! marginalia :pin "c68164c56485e1ef855c2d12e4393f5f55ca2b12")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (modulep! +icons)
  (package! all-the-icons-completion :pin "4da28584a1b36b222e0e78d46fd8d46bbd9116c7"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "61a88aec07669d0399bbc6699740975d0d5ff721"))
