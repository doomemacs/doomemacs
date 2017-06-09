;;; lang/org/autoload/babel.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org/edit-special-same-window ()
  (interactive)
  (let ((shackle-rules '(("^\\*Org Src" :align t :select t :regexp t :noesc t :same t))))
    (call-interactively #'org-edit-special)
    ;; FIXME too tightly coupled with doom-buffer-mode
    (when (fboundp 'solaire-mode)
      (solaire-mode +1))))
