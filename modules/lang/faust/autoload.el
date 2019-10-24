;;; lang/faust/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +faust-company-backend (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `faust-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend '+faust-company-backend))
    (prefix (and (derived-mode-p 'faust-mode)
                 (not (company-in-string-or-comment))
                 (or (company-grab-symbol-cons "\\." 1) 'stop)))
    (candidates (cl-remove-if-not (lambda (c) (string-prefix-p arg c))
                                  faust-keywords-all))))
