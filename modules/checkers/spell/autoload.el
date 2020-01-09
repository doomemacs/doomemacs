;;; checkers/spell/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
(defalias 'flyspell-mode! #'flyspell-mode)

(defvar +spell--flyspell-predicate-alist nil
  "TODO")

;;;###autodef
(defun set-flyspell-predicate! (modes predicate)
  "TODO"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes) +spell--flyspell-predicate-alist)
    (add-to-list '+spell--flyspell-predicate-alist (cons mode predicate))))

;;;###autoload
(defun +spell-init-flyspell-predicate-h ()
  "TODO"
  (when-let (pred (assq major-mode +spell--flyspell-predicate-alist))
    (setq-local flyspell-generic-check-word-predicate (cdr pred))))

;;;###autoload
(defun +spell-correction-at-point-p (&optional point)
  "TODO"
  (cl-loop for ov in (overlays-at (or point (point)))
           if (overlay-get ov 'flyspell-overlay)
           return t))
