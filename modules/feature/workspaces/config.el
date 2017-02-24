;;; feature/workspaces/config.el

;; `persp-mode' gives me workspaces, a workspace-restricted `buffer-list', and
;; file-based session persistence. I had used workgroups2 for this, but
;; abandoned it because of its instability and impact on performance.
;; `persp-mode' has proven faster and more reliable (and it's still maintained).
;;
;; I've disabled auto-load/save. I prefer that session persistence be manual.
;; You can save a session with :ss or `+workspace/save-session', and load the
;; last saved session with :sl or `+workspace/load-session'.
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

        ;; Don't auto-load on startup
        persp-auto-resume-time -1
        ;; Don't auto-save
        persp-auto-save-opt 0)

  (add-hook 'after-init-hook 'persp-mode)

  (define-key persp-mode-map [remap delete-window] '+workspace/close-window-or-workspace)

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
    (let ((project-name (projectile-project-name)))
      (if (+workspace-exists-p project-name)
          (+workspace/switch-to project-name)
        (+workspace/new project-name))))
  (add-hook 'projectile-before-switch-project-hook 'doom|new-workspace-on-project-change)

  ;; TODO Test per-frame perspectives

  ;; Be quiet when saving
  (defun +workspace*silence (orig-fn &rest args) (quiet! (apply orig-fn args)))
  (advice-add 'persp-save-state-to-file :around '+workspace*silence)

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
            (doom-popup-mode +1))))))
  (add-hook '+workspaces-load-session-hook '+workspaces|restore-popups))

