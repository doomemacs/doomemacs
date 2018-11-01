;;; ui/frame-workflow/config.el -*- lexical-binding: t; -*-

(def-package! frame-workflow
  :init
  (frame-workflow-mode 1)
  (map! :leader
        (:desc "frame"
          :prefix "<backtab>" :desc "Switch to frame" :n "." #'+frame-workflow/ivy-switch-frame)))

(after! counsel-projectile
  (setq counsel-projectile-switch-project-action #'frame-workflow-switch-directory-frame))
