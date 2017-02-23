;; -*- no-byte-compile: t; -*-
;;; feature/version-control/packages.el

;;; config.el
;; n/a

;;; +git
(package! browse-at-remote)
(package! git-gutter-fringe)
(package! gitconfig-mode)
(package! gitignore-mode)
(package! magit)
(when (featurep! :feature evil)
  (package! evil-magit))

;;; TODO +hg
