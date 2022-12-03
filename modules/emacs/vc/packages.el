;; -*- no-byte-compile: t; -*-
;;; emacs/vc/packages.el

(package! vc :built-in t)
(package! vc-annotate :built-in t)
(package! smerge-mode :built-in t)

(package! browse-at-remote :pin "cef26f2c063f2473af42d0e126c8613fe2f709e4")
(package! git-commit :pin "8a0cc83eff98489d3685b8585afdcebbb47c1393")
(package! git-timemachine
  ;; The original lives on codeberg.org; which has uptime issues.
  :recipe (:host github :repo "emacsmirror/git-timemachine")
  :pin "ca09684e94767cc0b2339b77b778b4de4f9d104f")
(package! git-modes :pin "eca3bb42ea8abed9ef8549b2ac91bbea445c5bb5")
