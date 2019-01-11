;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! dired-k)
(when (featurep! +ranger)
  (package! ranger))
(when (featurep! +icons)
  (package! all-the-icons-dired)
  (package! font-lock+ :recipe (:fetcher github :repo emacsmirror/font-lock-plus)))
