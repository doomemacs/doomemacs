;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(when (featurep! +snippets)
  (package! emacs-snippets
    :recipe (:fetcher github
             :repo "hlissner/emacs-snippets"
             :files ("*"))))
