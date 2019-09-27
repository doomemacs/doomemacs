;;; lang/plantuml/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +plantuml/install ()
  "Install plantuml.jar."
  (interactive)
  (if (file-exists-p plantuml-jar-path)
      (user-error "plantuml.jar already installed")
    (url-copy-file "https://downloads.sourceforge.net/project/plantuml/plantuml.jar"
                   plantuml-jar-path)))
