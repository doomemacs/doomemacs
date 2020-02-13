;;; tools/taskrunner/config.el -*- lexical-binding: t; -*-

(use-package! helm-taskrunner
  :when (featurep! :completion helm)
  :after (taskrunner)
  :init (helm-taskrunner-minor-mode t))

(use-package! ivy-taskrunner
  :when (featurep! :completion ivy)
  :after (taskrunner)
  :init (ivy-taskrunner-minor-mode t))
