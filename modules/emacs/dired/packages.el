;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "f9140b2c42151dca669003d685c9f079b2e3dc37")
(package! dired-git-info :pin "9461476a28a5fec0784260f6e318237c662c3430")
(package! diff-hl :pin "4a08b02afec1fc6b1e84de46cc34f75f6c9c3bcc")
(package! dired-rsync :pin "7940d9154d0a908693999b0e1ea351a6d365c93d")
(when (featurep! +ranger)
  (package! ranger :pin "2498519cb21dcd5791d240607a72a204d1761668"))
(when (featurep! +icons)
  (package! all-the-icons-dired :pin "5e9b097f9950cc9f86de922b07903a4e5fefc733"))
(package! fd-dired :pin "458464771bb220b6eb87ccfd4c985c436e57dc7e")
