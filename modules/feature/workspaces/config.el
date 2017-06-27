;;; feature/workspaces/config.el -*- lexical-binding: t; -*-

;; `persp-mode' gives me workspaces, a workspace-restricted `buffer-list', and
;; file-based session persistence. I used workgroups2 before this, but abandoned
;; it because it was unstable and slow; `persp-mode' is neither (and still
;; maintained).
;;
;; By default, sessions are autosaved, but not autoloaded. Use :ss or
;; `+workspace/save-session' to save, and :sl or `+workspace/load-session' to
;; load the last autosaved session. You can give sessions a custom name so they
;; can be loaded later.
;;
;; FYI persp-mode requires `workgroups' for file persistence in Emacs 24.4.

(defvar +workspaces-load-session-hook nil
  "A hook that runs when persp loads a new session.")

(defvar +workspaces-main "main"
  "The name of the primary and initial workspace, which cannot be deleted or
renamed.")


;;
;; Plugins
;;

(def-package! persp-mode
  :demand t
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
        persp-auto-save-opt (if noninteractive 0 1))

  (add-hook 'doom-init-hook #'+workspaces|init)
  (add-hook 'after-make-frame-functions #'+workspaces|init)

  ;; Defer delayed warnings even further, so they appear after persp-mode is
  ;; started and the main workspace is ready to display them.
  (remove-hook 'delayed-warnings-hook #'display-delayed-warnings)

  (defun +workspaces|init (&optional frame)
    (unless persp-mode
      (persp-mode +1)
      ;; Remap `buffer-list' to current workspace's buffers in
      ;; `doom-buffer-list'
      (advice-add #'doom-buffer-list :override #'+workspace-buffer-list))
    (let ((frame (or frame (selected-frame))))
      (unless noninteractive
        ;; The default perspective persp-mode makes (defined by
        ;; `persp-nil-name') is special and doesn't actually represent a real
        ;; persp object, so buffers can't really be assigned to it, among other
        ;; quirks. We create a *real* main workspace to fill this role.
        (unless (persp-with-name-exists-p +workspaces-main)
          (persp-add-new +workspaces-main))
        ;; Switch to it if we aren't auto-loading the last session
        (when (and (equal (safe-persp-name (get-current-persp)) persp-nil-name)
                   (= persp-auto-resume-time -1))
          (persp-frame-switch +workspaces-main frame)))
      (add-hook 'delayed-warnings-hook #'display-delayed-warnings t)))

  (define-key persp-mode-map [remap delete-window] #'+workspace/close-window-or-workspace)

  ;; Spawn a perspective for each new frame
  (setq persp-init-new-frame-behaviour-override nil
        persp-interactive-init-frame-behaviour-override
        (lambda (frame &optional _new-frame-p)
          (select-frame frame)
          (+workspace/new)
          (set-frame-parameter frame 'assoc-persp (+workspace-current-name))))

  (defun +workspaces*delete-frame-and-persp (frame)
    "Delete workspace associated with current frame IF it has no real buffers."
    (when (and (string= (or (frame-parameter frame 'assoc-persp) "")
                        (+workspace-current-name))
               (not (delq (doom-fallback-buffer) (doom-real-buffer-list))))
      (+workspace/delete persp-name)))
  (add-hook 'delete-frame-functions #'+workspaces*delete-frame-and-persp)

  (defun +workspaces*auto-add-buffer (buffer &rest _)
    "Auto-associate buffers with perspectives upon opening them. Allows a
perspective-specific buffer list via `+workspaces-buffer-list'."
    (when (and persp-mode
               (not persp-temporarily-display-buffer)
               (doom-real-buffer-p buffer))
      (persp-add-buffer buffer (get-current-persp) nil)
      (redisplay)))
  (advice-add #'switch-to-buffer :after #'+workspaces*auto-add-buffer)
  (advice-add #'display-buffer   :after #'+workspaces*auto-add-buffer)

  (defun +workspaces|create-project-workspace ()
    "Create a new workspace when switching project with projectile."
    (+workspace-switch (projectile-project-name) t))
  (add-hook 'projectile-before-switch-project-hook #'+workspaces|create-project-workspace)

  (defun +workspaces*autosave-real-buffers (orig-fn &rest args)
    "Don't autosave if no real buffers are open."
    (when (doom-real-buffer-list)
      (apply orig-fn args))
    t)
  (advice-add #'persp-asave-on-exit :around #'+workspaces*autosave-real-buffers))

