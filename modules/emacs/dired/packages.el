;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "83567d00affce66a4e501563eddd0bd436ac48d0")
(package! dired-git-info :pin "b47f2b0c3a6cb9b7a62a4ee2605a492e512d40a9")
(package! diff-hl :pin "2281a89a3ddc6616073da6f190dda08d23b18ba6")
(package! dired-rsync :pin "bfd5c155be1cb6b71c83e5f41116c81b6532b6d5")
(when (featurep! +ranger)
  (package! ranger :pin "d7c18370981c9e585bc0fb78f7e55033457ca643"))
(when (featurep! +icons)
  (package! all-the-icons-dired :pin "fc2dfa1e9eb8bf1c402a675e7089638d702a27a5"))
(package! fd-dired :pin "5622041068d5fa2f299dbc8aa91fece0ba260086")
