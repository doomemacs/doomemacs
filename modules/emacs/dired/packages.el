;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "cd052dfef602fe79d8dfbcf9f06e6da74412218b")
(package! dired-git-info :pin "b47f2b0c3a6cb9b7a62a4ee2605a492e512d40a9")
(package! diff-hl :pin "2281a89a3ddc6616073da6f190dda08d23b18ba6")
(package! dired-rsync :pin "bfd5c155be1cb6b71c83e5f41116c81b6532b6d5")
(when (featurep! +ranger)
  (package! ranger :pin "caf75f0060e503af078c7e5bb50d9aaa508e6f3e"))
(when (featurep! +icons)
  (package! all-the-icons-dired :pin "fc2dfa1e9eb8bf1c402a675e7089638d702a27a5"))
(package! fd-dired :pin "5622041068d5fa2f299dbc8aa91fece0ba260086")
