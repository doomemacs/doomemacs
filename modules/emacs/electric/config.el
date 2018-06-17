;;; emacs/electric/config.el -*- lexical-binding: t; -*-

;; Smarter, keyword-based electric-indent

(defvar-local +electric-indent-words '()
  "The list of electric words. Typing these will trigger reindentation of the
current line.")

;;
(after! electric
  (setq-default electric-indent-chars '(?\n ?\^?))

  (defun +electric-indent|char (_c)
    (when (and (eolp) +electric-indent-words)
      (save-excursion
        (backward-word)
        (looking-at-p (concat "\\<" (regexp-opt +electric-indent-words))))))
  (add-to-list 'electric-indent-functions #'+electric-indent|char nil #'eq))

