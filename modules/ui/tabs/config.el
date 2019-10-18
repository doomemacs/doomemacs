;;; ui/tabs/config.el -*- lexical-binding: t; -*-

(use-package! centaur-tabs
  :after-call after-find-file dired-initial-position-hook
  :init
  (setq centaur-tabs-height 28
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker t)

  :config
  (add-hook! 'centaur-tabs-mode-hook
    (defun +tabs-init-frames-h ()
      (dolist (frame (frame-list))
        (if (not centaur-tabs-mode)
            (set-frame-parameter frame 'buffer-predicate (frame-parameter frame 'old-buffer-predicate))
          (set-frame-parameter frame 'old-buffer-predicate (frame-parameter frame 'buffer-predicate))
          (set-frame-parameter frame 'buffer-predicate #'+tabs-buffer-predicate)))))

  (add-to-list 'window-persistent-parameters '(tab-buffers . t))

  (defun +tabs-window-buffer-list-fn ()
    (centaur-tabs-filter-out
     'centaur-tabs-hide-tab-cached
     (delq nil
           (cl-mapcar #'(lambda (b)
                          (cond
                           ;; Always include the current buffer.
                           ((eq (current-buffer) b) b)
                           ((buffer-file-name b) b)
                           ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                           ((buffer-live-p b) b)))
                      (window-parameter nil 'tab-buffers)))))

  (defun +tabs-buffer-groups-fn ()
    (list
     (cond ((or (string-equal "*" (substring (buffer-name) 0 1))
                (memq major-mode '(magit-process-mode
                                   magit-status-mode
                                   magit-diff-mode
                                   magit-log-mode
                                   magit-file-mode
                                   magit-blob-mode
                                   magit-blame-mode
                                   )))
            "Emacs")
           ((derived-mode-p 'eshell-mode)
            "EShell")
           ((derived-mode-p 'dired-mode)
            "Dired")
           ((centaur-tabs-get-group-name (current-buffer))))))

  (setq centaur-tabs-buffer-list-function #'+tabs-window-buffer-list-fn
        centaur-tabs-buffer-groups-function #'+tabs-buffer-groups-fn)

  (advice-add #'centaur-tabs-buffer-close-tab :override #'+tabs-kill-tab-maybe-a)
  (advice-add #'bury-buffer :around #'+tabs-bury-buffer-a)
  (advice-add #'kill-current-buffer :before #'+tabs-kill-current-buffer-a)
  (add-hook 'doom-switch-buffer-hook #'+tabs-add-buffer-h)
  (add-hook 'doom-switch-window-hook #'+tabs-new-window-h)

  (add-hook '+doom-dashboard-mode-hook #'centaur-tabs-local-mode)

  (map! (:map centaur-tabs-mode-map
          [remap delete-window] #'+tabs/close-tab-or-window
          [remap +workspace/close-window-or-workspace] #'+tabs/close-tab-or-window)
        (:after persp-mode
          :map persp-mode-map
          [remap delete-window] #'+tabs/close-tab-or-window
          [remap +workspace/close-window-or-workspace] #'+tabs/close-tab-or-window))

  (centaur-tabs-mode +1))
