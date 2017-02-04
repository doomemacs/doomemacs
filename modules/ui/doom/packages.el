;; -*- no-byte-compile: t; -*-
;;; ui/doom/packages.el

(package! beacon :demand t)

(package! doom-themes
  :ensure nil
  :demand t
  :load-path "~/work/plugins/emacs-doom-theme")
