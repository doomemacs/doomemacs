;;; lang/odin/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +odin/build ()
  "Build the current Odin project."
  (interactive)
  (let ((default-directory (or (doom-project-root) default-directory)))
    (compile +odin-build-command)))

;;;###autoload
(defun +odin/test ()
  "Test the current Odin project."
  (interactive)
  (let ((default-directory (or (doom-project-root) default-directory)))
    (compile +odin-test-command)))
