;;; config/literate/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-hook 'org-mode-hook #'+literate|enable-compile-on-save)

;;;###autoload
(defun +literate|enable-compile-on-save ()
  "TODO"
  (add-hook 'after-save-hook #'+literate|compile-on-save nil 'local))

;;;###autoload
(defun +literate|compile-on-save ()
  "TODO"
  (when (and (eq major-mode 'org-mode)
             buffer-file-name
             (file-in-directory-p buffer-file-name )))
  (+literate/compile))

;;;###autoload
(defun +literate/compile (&optional load)
  "TODO"
  (interactive "P")
  (+literate-compile load))
