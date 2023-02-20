;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "6f22ff129886a6b9292212f9897c65f919340aca")

(package! orderless :pin "d09aab37951b25627b96660f429eaec969d16d8a")

(package! consult :pin "ffaaf6da909dc9ff766e5a5f16eb265635aa6149")
(package! consult-dir :pin "ed8f0874d26f10f5c5b181ab9f2cf4107df8a0eb")
(when (modulep! :checkers syntax)
  (package! consult-flycheck :pin "c371996c571b7139ef4d9a8db142bf37a7ee826b"))
(package! embark :pin "5497a19eef92e4b82e2dcbcd26eb671227240c45")
(package! embark-consult :pin "5497a19eef92e4b82e2dcbcd26eb671227240c45")

(package! marginalia :pin "ccf573e2145d9deb9d734432351eefc87fc1bc16")

(package! wgrep :pin "edf768732a56840db6879706b64c5773c316d619")

(when (modulep! +icons)
  (package! all-the-icons-completion :pin "4da28584a1b36b222e0e78d46fd8d46bbd9116c7"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "f57b170b435ecb73027de00783c58e7cb46019a5"))
