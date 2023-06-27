;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "926234ab3fbe2b89e8c7ddfccecff518d73ac6ba")

(package! orderless :pin "e6784026717a8a6a7dcd0bf31fd3414f148c542e")

(package! consult :pin "511d8c0b072a660009fcba1f461dba9e21bc0cf0")
(package! consult-dir :pin "ed8f0874d26f10f5c5b181ab9f2cf4107df8a0eb")
(when (modulep! :checkers syntax)
  (package! consult-flycheck :pin "fda630411ad9219f45136310f671b44eaefafcab"))
(package! embark :pin "9b17d9a63b6960e026ad3c09a7871e0a3364e926")
(package! embark-consult :pin "9b17d9a63b6960e026ad3c09a7871e0a3364e926")

(package! marginalia :pin "b900ec5457068cd2b15b0e3600437f147c6bf636")

(package! wgrep :pin "3132abd3750b8c87cbcf6942db952acfab5edccd")

(when (modulep! +icons)
  (package! all-the-icons-completion :pin "b08f053cee444546ab44a05fd541f59e8bc8983b"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "7da6d648ff4202a48eb6647ee7dce8d65de48779"))
