;; -*- no-byte-compile: t; -*-
;;; term/vterm/packages.el

(package! vterm
  :built-in 'prefer
  :pin "a940dd2ee8a82684860e320c0f6d5e15d31d916f")

(when (featurep! +with-editor)
  (package! with-editor :pin "cfcbc2731e402b9169c0dc03e89b5b57aa988502"))
