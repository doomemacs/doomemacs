;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "cd052dfef602fe79d8dfbcf9f06e6da74412218b")
(package! dired-git-info :pin "b47f2b0c3a6cb9b7a62a4ee2605a492e512d40a9")
(package! diff-hl :pin "4c46b3b9851c85d15bff1e3ec92e5fc6043322bc")
(package! dired-rsync :pin "fb0f161ac3cce1b224f52547f5bc7e1dcd283191")
(when (featurep! +ranger)
  (package! ranger :pin "2498519cb21dcd5791d240607a72a204d1761668"))
(when (featurep! +icons)
  (package! all-the-icons-dired :pin "fc2dfa1e9eb8bf1c402a675e7089638d702a27a5"))
(package! fd-dired :pin "9fb966df33e7dde9360b8707f7a0197694f46abd")
