;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +intero) (load! "+intero"))
      ((featurep! +dante)  (load! "+dante")))


;;
;; Common plugins
;;

(def-package! hindent
  :hook (haskell-mode . hindent-mode))

(after! haskell-mode
  (setq haskell-indentation-electric-flag t)
  (set-repl-handler! 'haskell-mode #'switch-to-haskell)
  (add-to-list 'completion-ignored-extensions ".hi"))

