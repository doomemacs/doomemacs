;;; feature/file-templates/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +file-templates-get-short-path ()
  "TODO"
  (when (string-match "/modules/\\(.+\\)$" buffer-file-truename)
    (match-string 1 buffer-file-truename)))
