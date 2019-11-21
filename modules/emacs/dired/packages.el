;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl)
(package! dired-git-info)
(package! diff-hl)
(package! dired-rsync)
(when (featurep! +ranger)
  (package! ranger))
(when (featurep! +icons)
  (package! all-the-icons-dired))
(package! fd-dired)
