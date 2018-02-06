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
;; NOTE persp-mode requires `workgroups' for file persistence in Emacs 24.4.

(defvar +workspaces-main "main"
  "The name of the primary and initial workspace, which cannot be deleted or
renamed.")

;; FIXME actually use this for wconf bookmark system
(defvar +workspaces-data-file "_workspaces"
  "The basename of the file to store single workspace perspectives. Will be
stored in `persp-save-dir'.")


;;
;; Plugins
;;

(def-package! persp-mode
  :defer t
  :init
  (defun +workspaces|init ()
    ;; Remove default buffer predicate so persp-mode can put in its own
    (setq default-frame-alist (map-delete default-frame-alist 'buffer-predicate))
    (add-hook 'after-make-frame-functions #'+workspaces|init-frame)
    (require 'persp-mode)
    (unless (daemonp)
      (+workspaces|init-frame (selected-frame))))

  (defun +workspaces|init-frame (frame)
    "Make sure a main workspace exists and is switched to, if FRAME isn't in any
workspace. Also ensures that the *Warnings* buffer will be visible in main.

Uses `+workspaces-main' to determine the name of the main workspace."
    (unless persp-mode
      (persp-mode +1))
    (unless noninteractive
      (let (persp-before-switch-functions persp-activated-functions)
        (with-selected-frame frame
          ;; The default perspective persp-mode makes (defined by
          ;; `persp-nil-name') is special and doesn't actually represent a real
          ;; persp object, so buffers can't really be assigned to it, among other
          ;; quirks. We create a *real* main workspace to fill this role.
          (unless (persp-get-by-name +workspaces-main)
            (persp-add-new +workspaces-main))
          ;; Switch to it if we aren't auto-loading the last session
          (when (and (string= (safe-persp-name (get-current-persp)) persp-nil-name)
                     (= persp-auto-resume-time -1))
            (persp-frame-switch +workspaces-main frame)
            ;; We want to know where we are in every new daemon frame
            (when (daemonp)
              (run-at-time 0.1 nil #'+workspace/display))
            ;; The warnings buffer gets swallowed by creating
            ;; `+workspaces-main', so we display it manually, if it exists (fix
            ;; #319).
            (when-let* ((warnings (get-buffer "*Warnings*")))
              (save-excursion
                (display-buffer-in-side-window
                 warnings '((window-height . shrink-window-if-larger-than-buffer))))))))))

  (add-hook 'doom-init-hook #'+workspaces|init)
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-save-dir (concat doom-etc-dir "workspaces/")
        persp-set-last-persp-for-new-frames t
        persp-switch-to-added-buffer nil
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time -1 ; Don't auto-load on startup
        persp-auto-save-opt (if noninteractive 0 1)) ; auto-save on kill

  (add-hook 'persp-mode-hook #'+workspaces|init-persp-mode)
  ;; Modify `delete-window' to close the workspace if used on the last window
  (define-key persp-mode-map [remap delete-window] #'+workspace/close-window-or-workspace)
  (define-key persp-mode-map [remap evil-delete-window] #'+workspace/close-window-or-workspace)
  ;; only auto-save when real buffers are present
  (advice-add #'persp-asave-on-exit :around #'+workspaces*autosave-real-buffers)
  ;; For `doom/cleanup-session'
  (add-hook 'doom-cleanup-hook #'+workspaces|cleanup-unassociated-buffers)

  ;; per-frame workspaces
  (setq persp-init-frame-behaviour t
        persp-init-new-frame-behaviour-override nil
        persp-interactive-init-frame-behaviour-override #'+workspaces|associate-frame
        persp-emacsclient-init-frame-behaviour-override #'+workspaces|associate-frame)
  ;; delete frame associated with workspace, if it exists
  (add-hook 'delete-frame-functions #'+workspaces|delete-associated-workspace)

  ;; per-project workspaces
  (setq projectile-switch-project-action #'+workspaces|set-project-action)
  (advice-add #'counsel-projectile-switch-project-by-name :before #'+workspaces*switch-counsel-project-action)
  (add-hook 'projectile-after-switch-project-hook #'+workspaces|switch-to-project)

  ;;
  (defun +workspaces|init-persp-mode ()
    (cond (persp-mode
           ;; Ensure `persp-kill-buffer-query-function' is last in
           ;; kill-buffer-query-functions
           (remove-hook 'kill-buffer-query-functions 'persp-kill-buffer-query-function)
           (add-hook 'kill-buffer-query-functions 'persp-kill-buffer-query-function t)

           ;; Remap `buffer-list' to current workspace's buffers in
           ;; `doom-buffer-list'
           (advice-add #'switch-to-buffer :after #'+workspaces*auto-add-buffer)
           (advice-add #'display-buffer   :after #'+workspaces*auto-add-buffer)
           (advice-add #'doom-buffer-list :override #'+workspace-buffer-list))
          (t
           (advice-remove #'switch-to-buffer #'+workspaces*auto-add-buffer)
           (advice-remove #'display-buffer   #'+workspaces*auto-add-buffer)
           (advice-remove #'doom-buffer-list #'+workspace-buffer-list))))

  (defun +workspaces*auto-add-buffer (buffer &rest _)
    "Auto-associate buffers with perspectives upon opening them.

Allows a perspective-specific buffer list via `+workspaces-buffer-list'."
    (when (and persp-mode
               (not persp-temporarily-display-buffer)
               (doom-real-buffer-p buffer))
      (persp-add-buffer buffer (get-current-persp) nil)
      (force-mode-line-update t))))

