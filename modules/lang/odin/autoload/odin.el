;;; lang/odin/autoload/odin.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +odin-build-command "odin build . -debug"
  "Command used to build Odin projects.
Can be overridden per-project via .dir-locals.el.")
;;;###autoload
(put '+odin-build-command 'safe-local-variable #'stringp)

;;;###autoload
(defvar +odin-test-command "odin test ."
  "Command used to test Odin projects.
Can be overridden per-project via .dir-locals.el.")
;;;###autoload
(put '+odin-test-command 'safe-local-variable #'stringp)

;;;###autoload
(defun +odin/build ()
  "Build the current Odin project."
  (interactive)
  (let ((default-directory (or (doom-project-root) default-directory)))
    (save-some-buffers)
    (compile +odin-build-command)))

;;;###autoload
(defun +odin/test ()
  "Test the current Odin project."
  (interactive)
  (let ((default-directory (or (doom-project-root) default-directory)))
    (save-some-buffers)
    (compile +odin-test-command)))
