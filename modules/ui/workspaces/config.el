;;; ui/workspaces/config.el -*- lexical-binding: t; -*-

(use-package! tabspaces
  :hook (doom-init-ui . tabspaces-mode)
  :init
  (setq tabspaces-session-file (file-name-concat doom-profile-data-dir "workspaces.el")
        tab-bar-show 1)  ; hide if only one workspace

  :config
  (setq tabspaces-session t
        tabspaces-session-auto-restore (modulep! +auto)
        tabspaces-use-filtered-buffers-as-default t
        tabspaces-default-tab "Main"
        tabspaces-remove-to-default nil)

  ;; Delete the current workspace if closing the last open window
  (define-key! tabspaces-mode-map
    [remap delete-window] #'+workspaces/close-window-or-workspace
    [remap evil-window-delete] #'+workspaces/close-window-or-workspace)

  ;; Per-workspace `winner-mode' history
  (add-to-list 'window-persistent-parameters '(winner-ring . t))

  (defadvice! +workspaces--evil-alternate-buffer-a (&optional window)
    "Make `evil-alternate-buffer' ignore buffers outside the current workspace."
    :override #'evil-alternate-buffer
    (let* ((prev-buffers
            (if persp-mode
                (cl-remove-if-not #'tabspaces--local-buffer-p (window-prev-buffers)
                                  :key #'car)
              (window-prev-buffers)))
           (head (car prev-buffers)))
      (if (eq (car head) (window-buffer window))
          (cadr prev-buffers)
        head))))
