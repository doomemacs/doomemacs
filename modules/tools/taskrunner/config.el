;;; tools/taskrunner/config.el -*- lexical-binding: t; -*-

(after! taskrunner
  (set-popup-rule! taskrunner--buffer-name-regexp :quit t))

(use-package! helm-taskrunner
  :when (modulep! :completion helm)
  :defer t
  :config (helm-taskrunner-minor-mode +1))

(use-package! ivy-taskrunner
  :when (modulep! :completion ivy)
  :defer t
  :config (ivy-taskrunner-minor-mode +1))
