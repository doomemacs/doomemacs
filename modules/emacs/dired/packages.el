;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl)
(package! dired-k)
(when (featurep! +ranger)
  (package! ranger))
(when (featurep! +icons)
  (package! all-the-icons-dired))
