;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "8078b8cb940d6b2fe7578bc2e9c7cf1838cd092f")

(package! orderless :pin "6b86527b30ef96e047d97e314ac640a410891d1f")

(package! consult :pin "76aab86015c3d7628dbd5f92b2dd8ab9aeadac8d")
(package! compat :pin "056e3ccffc716990dcb7b33273453d5fce0402de")
(package! consult-dir :pin "8abf62df088de87175e98adf8f6f5fb93515004c")
(when (modulep! :checkers syntax)
  (package! consult-flycheck :pin "9b40f136c017fadf6239d7602d16bf73b4ad5198"))

(package! embark :pin "3add321d7442973413fb92a4052f8d0ad6915829")
(package! embark-consult :pin "3add321d7442973413fb92a4052f8d0ad6915829")

(package! marginalia :pin "7f5bf7818b8c5a88cc3e7011d561655b287570e3")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (modulep! +icons)
  (package! all-the-icons-completion :pin "286e2c064a1298be0d8d4100dc91d7a7a554d04a"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "46aa1dffd18dc9500c04f0b61524b0625b7b5429"))
