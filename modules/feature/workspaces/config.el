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

(defvar +workspaces-main "main"
  "The name of the primary and initial workspace, which cannot be deleted or
renamed.")


;;
;; Plugins
;;

(def-package! persp-mode :demand t
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-name "nil"
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-save-dir (concat doom-cache-dir "workspaces/")
        persp-set-last-persp-for-new-frames nil
        persp-switch-to-added-buffer nil
        persp-remove-buffers-from-nil-persp-behaviour nil
        ;; Don't restore winconf on new frames
        persp-init-frame-behaviour t
        persp-init-new-frame-behaviour-override 'auto-temp
        ;; Don't auto-load on startup
        persp-auto-resume-time -1
        ;; auto-save on kill
        persp-auto-save-opt 1)

  (defun +workspaces|init (&rest _)
    (unless persp-mode
      (persp-mode +1))
    ;; The default perspective persp-mode makes (defined by `persp-nil-name') is
    ;; special and doesn't actually represent a real persp object, so buffers
    ;; can't really be assigned to it, among other quirks. We create a *real*
    ;; main workspace to fill this role.
    (persp-add-new +workspaces-main)
    ;; Switch to it if we aren't auto-loading the last session
    (when (or (= persp-auto-resume-time -1)
              (equal (safe-persp-name (get-current-persp)) persp-nil-name))
      (persp-frame-switch +workspaces-main)))

  (add-hook! 'after-init-hook
    (if (display-graphic-p)
        (+workspaces|init)
      (add-hook 'after-make-frame-functions #'+workspaces|init)))

  (define-key persp-mode-map [remap delete-window] #'+workspace/close-window-or-workspace)

  ;; Spawn a perspective for each new frame
  (setq persp-init-new-frame-behaviour-override nil
        persp-interactive-init-frame-behaviour-override
        (lambda (frame &optional new-frame-p)
          (select-frame frame)
          (+workspace/new)
          (set-frame-parameter frame 'assoc-persp (+workspace-current-name))))

  (defun +workspaces*delete-frame-and-persp (frame)
    "Delete workspace associated with current frame IF it has no real buffers."
    (when (and (string= (or (frame-parameter frame 'assoc-persp) "") (+workspace-current-name))
               (not (delq (doom-fallback-buffer) (doom-real-buffers-list))))
      (+workspace/delete persp-name)))
  (add-hook 'delete-frame-functions #'+workspaces*delete-frame-and-persp)

  (defun +workspaces*auto-add-buffer (buffer &rest _)
    "Auto-associate buffers with perspectives upon opening them. Allows a
perspective-specific buffer list via `doom-buffer-list'."
    (when (and persp-mode
               (not persp-temporarily-display-buffer)
               (doom-real-buffer-p buffer))
      (persp-add-buffer buffer (get-current-persp) nil)
      (redisplay)))
  (advice-add #'switch-to-buffer :after #'+workspaces*auto-add-buffer)
  (advice-add #'display-buffer   :after #'+workspaces*auto-add-buffer)

  (defun +workspaces|workspace-per-project ()
    "Create a new workspace when switching project with projectile."
    (+workspace-switch (projectile-project-name) t))
  (add-hook 'projectile-before-switch-project-hook #'+workspaces|workspace-per-project)

  (defun +workspaces|restore-popups (&rest _)
    "Restore popup windows when loading a perspective from file."
    (dolist (window (window-list))
      (when-let (plist (window-parameter window 'popup))
        (with-selected-window window
          (unless doom-popup-mode
            (setq-local doom-popup-rules plist)
            (doom-popup-mode +1))))))
  (advice-add #'persp-load-state-from-file :after #'+workspaces|restore-popups)

  (defun +workspaces*autosave-real-buffers (orig-fn &rest args)
    "Don't autosave if no real buffers are open."
    (when (doom-real-buffers-list)
      (apply orig-fn args))
    t)
  (advice-add #'persp-asave-on-exit :around #'+workspaces*autosave-real-buffers))

