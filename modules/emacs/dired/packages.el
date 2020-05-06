;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "83567d00affce66a4e501563eddd0bd436ac48d0")
(package! dired-git-info :pin "b47f2b0c3a6cb9b7a62a4ee2605a492e512d40a9")
(package! diff-hl :pin "a625033fb1dde83f6e4c2fc21f632b22ec34b609")
(package! dired-rsync :pin "bfd5c155be1cb6b71c83e5f41116c81b6532b6d5")
(when (featurep! +ranger)
  (package! ranger :pin "ae9b3816a6da927cca5beb62c45400103797a2da"))
(when (featurep! +icons)
  (package! all-the-icons-dired :pin "fc2dfa1e9eb8bf1c402a675e7089638d702a27a5"))
(package! fd-dired :pin "001cc95effdd5c4d9974b3f2c40b2ddf1f0e3de2")
