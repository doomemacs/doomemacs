;;; org/org-babel/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-babel/edit (arg)
  "Edit the source block at point in a popup.

If ARG is non-nil (universal argument), use the current window."
  (interactive "P")
  (if arg
      (call-interactively #'org-edit-special)
    (with-popup-rules! (("^\\*Org Src" :regexp t :select t :noesc t :same t))
      (call-interactively #'org-edit-special))))
