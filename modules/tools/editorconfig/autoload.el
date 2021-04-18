;;; tools/editorconfig/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-editorconfig-indent-var! (mode &rest vars)
  "Add (MODE VARS...) to `editorconfig-indentation-alist'."
  (after! editorconfig
    (nconc editorconfig-indentation-alist (cons mode vars))))
