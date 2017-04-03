;;; feature/workspaces/config.el

;; `persp-mode' gives me workspaces, a workspace-restricted `buffer-list', and
;; file-based session persistence. I had used workgroups2 before this, but
;; abandoned it because of its instability and impact on performance.
;; `persp-mode' has proven faster and more reliable (and it's still maintained).
;;
;; By default, sessions are auto-saved, but not auto-loaded. Use :ss or
;; `+workspace/save-session' to save, and :sl or `+workspace/load-session' to
;; load the last autosaved session. You can give sessions a custom name so they
;; can be loaded much later.
;;
;; Note: persp-mode requires `workgroups' for file persistence in Emacs 24.4.

(defvar +workspaces-load-session-hook nil
  "A hook that runs when persp loads a new session.")


(def-package! persp-mode :demand t
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-name "main"
        persp-auto-save-fname "autosave"
        persp-save-dir (concat doom-cache-dir "workspaces/")
        persp-set-last-persp-for-new-frames nil
        persp-switch-to-added-buffer nil
        ;; Don't restore winconf on new frames
        persp-init-frame-behaviour t
        persp-init-new-frame-behaviour-override 'auto-temp
        ;; Don't auto-load on startup
        persp-auto-resume-time -1
        ;; auto-save on kill
        persp-auto-save-opt 1)

  (add-hook 'after-init-hook 'persp-mode)

  (define-key persp-mode-map [remap delete-window] '+workspace/close-window-or-workspace)

  ;; Per-frame perspectives
  (setq persp-init-new-frame-behaviour-override nil
        persp-interactive-init-frame-behaviour-override
        (lambda (frame &optional new-frame-p)
          (select-frame frame)
          (+workspace/new)
          (set-frame-parameter frame 'assoc-persp (+workspace-current-name))))
  ;; Delete workspace associated with current frame IF it has no real buffers.
  (defun +workspaces*delete-frame-and-persp (frame)
    (when (and (string= (or (frame-parameter frame 'assoc-persp) "") (+workspace-current-name))
               (not (delq (doom-fallback-buffer) (doom-real-buffers-list))))
      (+workspace/delete persp-name)))
  (add-hook 'delete-frame-functions '+workspaces*delete-frame-and-persp)

  ;; Auto-add buffers when opening them. Allows a perspective-specific buffer list.
  (defun +workspaces*auto-add-buffer (buffer &rest _)
    (when (and persp-mode
               (not persp-temporarily-display-buffer)
               (doom-real-buffer-p buffer))
      (persp-add-buffer buffer (get-current-persp) nil)
      (redisplay)))
  (advice-add 'switch-to-buffer :after '+workspaces*auto-add-buffer)
  (advice-add 'display-buffer   :after '+workspaces*auto-add-buffer)

  ;; Create a new workspace on project switch
  (defun doom|new-workspace-on-project-change ()
    (+workspace-switch (projectile-project-name) t))
  (add-hook 'projectile-before-switch-project-hook 'doom|new-workspace-on-project-change)

  ;; Add a hook to session loading
  (defun +workspaces*reinit-popups (&rest _)
    (run-hook-with-args '+workspaces-load-session-hook (window-list)))
  (advice-add 'persp-load-state-from-file :after '+workspaces*reinit-popups)

  ;; Restore popups on load
  (defun +workspaces|restore-popups (windows)
    (dolist (window windows)
      (when-let (plist (window-parameter window 'popup))
        (with-selected-window window
          (unless doom-popup-mode
            (setq-local doom-popup-rules plist)
            (doom-popup-mode +1))))))
  (add-hook '+workspaces-load-session-hook '+workspaces|restore-popups))

