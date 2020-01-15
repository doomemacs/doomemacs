;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "83567d00affce66a4e501563eddd0bd436ac48d0")
(package! dired-git-info :pin "b47f2b0c3a6cb9b7a62a4ee2605a492e512d40a9")
(package! diff-hl :pin "fb9eb1cd3c4c6ed24b93de1a7cfb369d2983be74")
(package! dired-rsync :pin "698294cbd4b731abcb617f29aa133bc9c60b2651")
(when (featurep! +ranger)
  (package! ranger :pin "c3f349e52f5c50926dc0f285c97676934f50bc18"))
(when (featurep! +icons)
  (package! all-the-icons-dired :pin "980b7747d6c4a7992a1ec56afad908956db0a519"))
(package! fd-dired :pin "fd4c3f490b0b6727592b85f1635e57638dec8f91")
