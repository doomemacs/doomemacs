;;; tools/flyspell/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
(defalias 'flyspell-mode! #'flyspell-mode)

(defvar +flyspell--predicate-alist nil
  "TODO")

;;;###autodef
(defun set-flyspell-predicate! (modes predicate)
  "TODO"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes) +flyspell--predicate-alist)
    (add-to-list '+flyspell--predicate-alist (cons mode predicate))))

;;;###autoload
(defun +flyspell|init-predicate ()
  "TODO"
  (when-let* ((pred (assq major-mode +flyspell--predicate-alist)))
    (setq-local flyspell-generic-check-word-predicate (cdr pred))))

;;;###autoload
(defun +flyspell-correction-at-point-p (&optional point)
  "TODO"
  (cl-loop for ov in (overlays-at (or point (point)))
           if (overlay-get ov 'flyspell-overlay)
           return t))
