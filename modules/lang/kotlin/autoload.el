;;; lang/kotlin/autoload.el -*- lexical-binding: t; -*-

;;;autoload
(defun +kotlin-locate-gradlew-file ()
  "Gradlew file location for this project."
  (locate-dominating-file buffer-file-name "gradlew"))

;;;###autoload
(defun +kotlin/run-gradlew (command)
  "Run gradlew in this project."
  (interactive "sCommand: ")
  (let ((default-directory (+kotlin-locate-gradlew-file))
        (compilation-read-command nil)
        (compile-command (format "sh gradlew %s" command)))
    (call-interactively #'compile)))
