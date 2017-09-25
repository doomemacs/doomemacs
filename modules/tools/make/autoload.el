;;; tools/make/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +make/run ()
  "Run a make task in the current project."
  (interactive)
  (require 'makefile-executor)
  (let* ((buffer-file (or buffer-file-name default-directory))
         (makefile-dir (locate-dominating-file buffer-file "Makefile")))
    (unless makefile-dir
      (user-error "No makefile found in this project."))
    (let ((default-directory makefile-dir))
      (makefile-executor-execute-target
       (expand-file-name "Makefile")))))
