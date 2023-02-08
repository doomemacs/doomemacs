;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "f303790546edecc67aa3bd5e23c68f982f1345dd")

(package! orderless :pin "ae849b3d9f8c8a777e05816321ed2b00e8304447")

(package! consult :pin "b22a7de62ee4adf766be2f867dee8b6980902bba")
(package! compat :pin "2bedcb5ea91914e75d4905bc53e537b33f8f51e9")
(package! consult-dir :pin "ed8f0874d26f10f5c5b181ab9f2cf4107df8a0eb")
(when (modulep! :checkers syntax)
  (package! consult-flycheck :pin "51b1b48e8dad314f9c9d963376f2ea8de94b97f2"))
(package! embark :pin "4882b395cef98a517d530ffe483aa0dc7201158c")
(package! embark-consult :pin "4882b395cef98a517d530ffe483aa0dc7201158c")

(package! marginalia :pin "6d48ed54be87969e3ce53a24dbc63ec72ec6a91a")

(package! wgrep :pin "edf768732a56840db6879706b64c5773c316d619")

(when (modulep! +icons)
  (package! all-the-icons-completion :pin "4da28584a1b36b222e0e78d46fd8d46bbd9116c7"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "790f74b49d5309dc2f0e6a438e2e89007d591d07"))
