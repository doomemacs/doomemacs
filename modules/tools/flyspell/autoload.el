;;; tools/flyspell/autoload.el -*- lexical-binding: t; -*-

(defvar +flyspell--predicate-alist nil
  "TODO")

;;;###autodef
(defun set-flyspell-predicate! (modes predicate)
  "TODO"
  (dolist (mode (doom-enlist modes) +flyspell--predicate-alist)
    (add-to-list '+flyspell--predicate-alist (cons mode predicate))))

;;;###autoload
(defun +flyspell|init-predicate ()
  "TODO"
  (when-let* ((pred (assq major-mode +flyspell--predicate-alist)))
    (setq-local flyspell-generic-check-word-predicate (cdr pred))))
