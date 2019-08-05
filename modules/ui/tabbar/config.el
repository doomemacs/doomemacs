;;; ui/tabbar/config.el -*- lexical-binding: t; -*-

(def-package! centaur-tabs
  :after-call (after-find-file dired-initial-position-hook)
  :config
  (setq centaur-tabs-height 28
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker t)

  (defun +tabbar|init-frames ()
    (dolist (frame (frame-list))
      (if (not centaur-tabs-mode)
          (set-frame-parameter frame 'buffer-predicate (frame-parameter frame 'old-buffer-predicate))
        (set-frame-parameter frame 'old-buffer-predicate (frame-parameter frame 'buffer-predicate))
        (set-frame-parameter frame 'buffer-predicate #'+tabbar-buffer-predicate))))
  (add-hook 'centaur-tabs-mode-hook #'+tabbar|init-frames)

  (setq centaur-tabs-buffer-list-function #'+tabbar-window-buffer-list
        centaur-tabs-buffer-groups-function #'+tabbar-buffer-groups)

  (advice-add #'centaur-tabs-buffer-close-tab :override #'+tabbar*kill-tab-maybe)
  (advice-add #'bury-buffer :around #'+tabbar*bury-buffer)
  (advice-add #'kill-current-buffer :before #'+tabbar*kill-current-buffer)
  (add-hook 'doom-switch-buffer-hook #'+tabbar|add-buffer)
  (add-hook 'doom-switch-window-hook #'+tabbar|new-window)

  (add-hook '+doom-dashboard-mode-hook #'centaur-tabs-local-mode)

  (map! (:map centaur-tabs-mode-map
          [remap delete-window] #'+tabbar/close-tab-or-window
          [remap +workspace/close-window-or-workspace] #'+tabbar/close-tab-or-window)
        (:after persp-mode
          :map persp-mode-map
          [remap delete-window] #'+tabbar/close-tab-or-window
          [remap +workspace/close-window-or-workspace] #'+tabbar/close-tab-or-window))

  (centaur-tabs-mode +1))
