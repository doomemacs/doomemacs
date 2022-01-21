;;; config/tutorial/autoload/tutorial.el -*- lexical-binding: t; -*-

;;;###autoload
(defun take-the-tutorial! ()
  "Tutorial entry point."
  (interactive)
  (with-current-buffer (get-buffer-create "*doom-tutorial*")
    (switch-to-buffer (current-buffer))
    (insert "TODO: this needs some work")))
