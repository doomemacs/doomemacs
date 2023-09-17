;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "f9140b2c42151dca669003d685c9f079b2e3dc37")
(package! dired-git-info :pin "9461476a28a5fec0784260f6e318237c662c3430")
(package! dired-rsync :pin "7940d9154d0a908693999b0e1ea351a6d365c93d")
(when (modulep! +ranger)
  (package! ranger :pin "2498519cb21dcd5791d240607a72a204d1761668"))
(when (modulep! +dirvish)
  (package! dirvish :pin "4fe9c00894304e99aca22ae4b6b656fe94b8f927"))
(when (and (modulep! +icons)
           (not (modulep! +dirvish)))
  (package! nerd-icons-dired :pin "4a068884bf86647d242c3adc8320cd603e15dac3"))
(package! fd-dired :pin "458464771bb220b6eb87ccfd4c985c436e57dc7e")
