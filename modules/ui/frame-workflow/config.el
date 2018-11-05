;;; ui/frame-workflow/config.el -*- lexical-binding: t; -*-

(def-package! frame-workflow
  :init
  (frame-workflow-mode 1)
  (frame-purpose-mode 1)
  (map! :leader
        (:desc "frame" :prefix [tab] :desc "Make subject frame for current buffer"
          :n [tab] #'+frame-workflow/make-frame-current-major-mode)))

(after! counsel-projectile
  (setq counsel-projectile-switch-project-action
        #'frame-workflow-switch-directory-frame))

(after! ivy
  (map! :leader
        (:desc "frame" :prefix [tab] :desc "Switch to frame" :n "." #'+frame-workflow/ivy-switch-frame)))

(after! helm
  (autoload 'helm-frame-workflow)
  (map! :leader
        (:desc "frame"
          :prefix [tab] :desc "Switch to frame" :n "." #'helm-frame-workflow)))
