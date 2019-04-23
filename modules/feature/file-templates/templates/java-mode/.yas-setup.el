(defun yas-java-project-package ()
  (or (and (eq major-mode 'java-mode)
           (+java-current-package))
      ""))

(defun yas-java-class-name ()
  (or (and (eq major-mode 'java-mode)
           (+java-current-class))
      ""))
