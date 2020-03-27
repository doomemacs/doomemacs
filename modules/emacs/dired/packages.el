;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "83567d00af")
(package! dired-git-info :pin "b47f2b0c3a")
(package! diff-hl :pin "2cf8b489f3")
(package! dired-rsync :pin "bfd5c155be")
(when (featurep! +ranger)
  (package! ranger :pin "ae9b3816a6"))
(when (featurep! +icons)
  (package! all-the-icons-dired :pin "816987d339"))
(package! fd-dired :pin "fd4c3f490b")
