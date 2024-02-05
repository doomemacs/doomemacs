;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "f6d599c30875ab4894c1deab9713ff2faea54e06")
(package! dired-git-info :pin "6b6f2a5d716debba9a7dcac623d5a1e4c799eb62")
(package! dired-rsync :pin "5bcb851f3bf9c4f7c07299fcc25be7c408a68cda")
(when (modulep! +ranger)
  (package! ranger :pin "2498519cb21dcd5791d240607a72a204d1761668"))
(when (modulep! +dirvish)
  (package! dirvish :pin "119f9f59a618bb7b476c93e9ab1d7542c5c1df41"))
(when (and (modulep! +icons)
           (not (modulep! +dirvish)))
  (package! nerd-icons-dired :pin "c1c73488630cc1d19ce1677359f614122ae4c1b9"))
(package! fd-dired :pin "458464771bb220b6eb87ccfd4c985c436e57dc7e")
