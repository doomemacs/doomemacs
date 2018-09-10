;;; ui/fci/autoload.el -*- lexical-binding: t; -*-

(defvar-local +fci-last-state nil)

;;;###autoload
(defun +fci|disable-when-company-activates (&rest ignore)
  "TODO"
  (when (setq +fci-last-state (bound-and-true-p fci-mode))
    (fci-mode -1)))

;;;###autoload
(defun +fci|enable-when-company-deactivates (&rest ignore)
  "TODO"
  (when +fci-last-state
    (fci-mode 1)))
