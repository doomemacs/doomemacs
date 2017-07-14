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

  ;; Bootstrap
  (add-hook 'doom-init-hook #'+workspaces|init)
  (add-hook 'after-make-frame-functions #'+workspaces|init)

  (define-key persp-mode-map [remap delete-window] #'+workspace/close-window-or-workspace)

  ;; per-frame workspaces
  (setq persp-init-new-frame-behaviour-override nil
        persp-interactive-init-frame-behaviour-override #'+workspace-on-new-frame)
  (add-hook 'projectile-before-switch-project-hook #'+workspaces|create-project-workspace)
  (add-hook 'delete-frame-functions #'+workspaces|delete-associated-workspace-maybe)

  ;; only auto-save when real buffers are present
  (advice-add #'persp-asave-on-exit :around #'+workspaces*autosave-real-buffers)

  (defun +workspaces|on-persp-mode ()
    ;; Remap `buffer-list' to current workspace's buffers in `doom-buffer-list'
    (if persp-mode
        (advice-add #'doom-buffer-list :override #'+workspace-buffer-list)
      (advice-remove #'doom-buffer-list #'+workspace-buffer-list)))
  (add-hook 'persp-mode-hook #'+workspaces|on-persp-mode)

  ;; Defer delayed warnings even further, so they appear after persp-mode is
  ;; started and the main workspace is ready to display them. Otherwise, warning
  ;; buffers will be hidden on startup.
  (remove-hook 'delayed-warnings-hook #'display-delayed-warnings)
  (defun +workspaces|init (&optional frame)
    (unless persp-mode
      (persp-mode +1))
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

  (defun +workspaces*auto-add-buffer (buffer &rest _)
    "Auto-associate buffers with perspectives upon opening them.

Allows a perspective-specific buffer list via `+workspaces-buffer-list'."
    (when (and persp-mode
               (not persp-temporarily-display-buffer)
               (doom-real-buffer-p buffer))
      (persp-add-buffer buffer (get-current-persp) nil)
      (redisplay)))
  (advice-add #'switch-to-buffer :after #'+workspaces*auto-add-buffer)
  (advice-add #'display-buffer   :after #'+workspaces*auto-add-buffer))

