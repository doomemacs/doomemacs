;; -*- no-byte-compile: t; -*-
;;; ui/fill-column/packages.el

;; `hl-fill-column' is only used on Emacs versions before 27
(unless (fboundp 'display-fill-column-indicator-mode)
  (package! hl-fill-column :pin "5782a91ba0182c4e562fa0db6379ff9dd472856b"))
