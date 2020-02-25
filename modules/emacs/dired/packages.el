;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "83567d00af")
(package! dired-git-info :pin "b47f2b0c3a")
(package! diff-hl :pin "fb9eb1cd3c")
(package! dired-rsync :pin "698294cbd4")
(when (featurep! +ranger)
  (package! ranger :pin "af6f781a60"))
(when (featurep! +icons)
  (package! all-the-icons-dired :pin "980b7747d6"))
(package! fd-dired :pin "fd4c3f490b")
