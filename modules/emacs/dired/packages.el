;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "4ca32658aebaf2335f0368a0fd08f52eb1aee960")
(package! dired-git-info :pin "9461476a28a5fec0784260f6e318237c662c3430")
(package! diff-hl :pin "40c89a7b0d2590b1e094128f3f1d6dc87bf14ce9")
(package! dired-rsync :pin "7940d9154d0a908693999b0e1ea351a6d365c93d")
(when (featurep! +ranger)
  (package! ranger :pin "2498519cb21dcd5791d240607a72a204d1761668"))
(when (featurep! +icons)
  (package! all-the-icons-dired :pin "a758766878b6e8b9eaaf41d68599a2df99e37f48"))
(package! fd-dired :pin "458464771bb220b6eb87ccfd4c985c436e57dc7e")
