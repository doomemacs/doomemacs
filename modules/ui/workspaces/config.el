;;; ui/workspaces/config.el -*- lexical-binding: t; -*-

(defvar +workspaces-dir (file-name-concat doom-profile-data-dir "workspaces/")
  "The directory where workspace session files are stored.")


;;
;;; Packages

(use-package! tabspaces
  :hook (doom-init-ui . tabspaces-mode)
  :init
  (setq tabspaces-session-file (file-name-concat +workspaces-dir "session.el"))
  (if (modulep! +tabs)
      (setq tab-bar-show 1)  ; hide if only one workspace
    (setq tabspaces-echo-area-enable t))

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
  (add-to-list 'window-persistent-parameters '(winner-ring . t)))
