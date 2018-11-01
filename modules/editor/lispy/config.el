;;; editor/lispy/config.el -*- lexical-binding: t; -*-

(def-package! lispy
  :hook ((common-lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (racket-mode . lispy-mode)
         (hy-mode . lispy-mode)
         (lfe-mode . lispy-mode)
         (clojure-mode . lispy-mode))
  :config
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode))

(def-package! lispyville
  :when (featurep! :feature evil)
  :hook (lispy-mode . lispyville-mode)
  :config
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
     (escape insert))))
