;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "4ca32658aebaf2335f0368a0fd08f52eb1aee960")
(package! dired-git-info :pin "9461476a28a5fec0784260f6e318237c662c3430")
(package! diff-hl :pin "1af31fe1c177646ef7419c95b5d9c25655187ceb")
(package! dired-rsync :pin "fb0f161ac3cce1b224f52547f5bc7e1dcd283191")
(when (featurep! +ranger)
  (package! ranger :pin "2498519cb21dcd5791d240607a72a204d1761668"))
(when (featurep! +icons)
  (package! all-the-icons-dired :pin "f401fe289cd93936e7747b1541aa98117b7ca96f"))
(package! fd-dired :pin "7d18938751d047eef18bfb5975195419f0d1e2d3")
