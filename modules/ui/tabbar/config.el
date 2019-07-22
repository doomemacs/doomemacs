;;; ui/tabbar/config.el -*- lexical-binding: t; -*-

(def-package! centaur-tabs
  :after-call (after-find-file dired-initial-position-hook)
  :config
  (setq centaur-tabs-height 28
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker t)

  (add-hook 'centaur-tabs-mode-hook
    (defun +tabbar-init-frames-h ()
      (dolist (frame (frame-list))
        (if (not centaur-tabs-mode)
            (set-frame-parameter frame 'buffer-predicate (frame-parameter frame 'old-buffer-predicate))
          (set-frame-parameter frame 'old-buffer-predicate (frame-parameter frame 'buffer-predicate))
          (set-frame-parameter frame 'buffer-predicate #'+tabbar-buffer-predicate)))))

  (setq centaur-tabs-buffer-list-function #'+tabbar-window-buffer-list-fn
        centaur-tabs-buffer-groups-function #'+tabbar-buffer-groups-fn)

  (advice-add #'centaur-tabs-buffer-close-tab :override #'+tabbar-kill-tab-maybe-a)
  (advice-add #'bury-buffer :around #'+tabbar-bury-buffer-a)
  (advice-add #'kill-current-buffer :before #'+tabbar-kill-current-buffer-a)
  (add-hook 'doom-switch-buffer-hook #'+tabbar-add-buffer-h)
  (add-hook 'doom-switch-window-hook #'+tabbar-new-window-h)

  (add-hook '+doom-dashboard-mode-hook #'centaur-tabs-local-mode)

  (map! (:map centaur-tabs-mode-map
          [remap delete-window] #'+tabbar/close-tab-or-window
          [remap +workspace/close-window-or-workspace] #'+tabbar/close-tab-or-window)
        (:after persp-mode
          :map persp-mode-map
          [remap delete-window] #'+tabbar/close-tab-or-window
          [remap +workspace/close-window-or-workspace] #'+tabbar/close-tab-or-window))

  (centaur-tabs-mode +1))
