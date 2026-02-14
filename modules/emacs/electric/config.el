;;; emacs/electric/config.el -*- lexical-binding: t; -*-

;; Smarter, keyword-based electric-indent

(defvar-local +electric-indent-words '()
  "The list of electric words. Typing these will trigger reindentation of the
current line.")

;;
(after! electric
  (setq-default electric-indent-chars '(?\n ?\^?))

  (add-hook! 'electric-indent-functions
    (defun +electric-indent-char-fn (_c)
      (when (and (eolp) +electric-indent-words)
        (save-excursion
          (backward-word)
          (looking-at-p (concat "\\<" (regexp-opt +electric-indent-words)))))))

  ;; Fix #5051: Ensure electric-quote's handler runs before smartparens' on
  ;; `post-self-insert-hook', so that `electric-quote-inhibit-functions'
  ;; (e.g. `org-in-src-block-p') is respected in org src blocks.
  (add-hook! 'electric-quote-mode-hook
    (defun +electric-quote-reorder-post-self-insert-h ()
      (when electric-quote-mode
        (remove-hook 'post-self-insert-hook #'electric-quote-post-self-insert-function)
        (add-hook 'post-self-insert-hook #'electric-quote-post-self-insert-function -50)))))
