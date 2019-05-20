;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl)
(package! dired-k)
(package! dired-rsync)
(when (featurep! +ranger)
  (package! ranger))
(when (featurep! +icons)
  (package! all-the-icons-dired))
