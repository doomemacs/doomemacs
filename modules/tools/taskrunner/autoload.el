;;; app/taskrunner/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +taskrunner/project-tasks ()
  "Invokes `ivy-taskrunner' or `helm-tasksrunner', depending on which is
available."
  (interactive)
  (cond ((modulep! :completion ivy) (ivy-taskrunner))
        ((modulep! :completion helm) (helm-taskrunner))))
