;;; editor/lispyville/config.el -*- lexical-binding: t; -*-

(def-package! lispyville
  :when (featurep! :feature evil)
  :init
  (lispyville-set-key-theme
   '((operators normal)
     c-w
     (prettify insert)
     (atom-movement normal visual)
     slurp/barf-lispy
     (wrap normal insert)
     additional
     additional-insert
     (additional-wrap normal insert)
     (escape insert))
  :hook ((common-lisp-mode . lispyville-mode)
          (emacs-lisp-mode . lispyville-mode)
          (scheme-mode . lispyville-mode)
          (racket-mode . lispyville-mode)
          (hy-mode . lispyville-mode)
          (lfe-mode . lispyville-mode)
          (clojure-mode . lispyville-mode))
  :config
  (add-hook 'lispyville-mode-hook #'turn-off-smartparens-mode)))
