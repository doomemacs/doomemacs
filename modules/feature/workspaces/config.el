;;; feature/workspaces/config.el

;; `persp-mode' gives me workspaces, a workspace-restricted `buffer-list', and
;; file-based session persistence. I had used workgroups2 for this, but
;; abandoned it because of its instability and impact on performance.
;; `persp-mode' has proven faster and more reliable (and it's still maintained).
;;
;; Note: persp-mode requires `workgroups' for file persistence in Emacs 24.4.

(defvar +workspaces-load-session-hook nil
  "A hook that runs when persp loads a new session.")


(@use-package persp-mode :demand t
  :init
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-name "main"
        persp-auto-save-fname "autosave"
        persp-save-dir (concat doom-cache-dir "workspaces/")
        persp-set-last-persp-for-new-frames nil
        persp-auto-resume-time (if (display-graphic-p) 0.01 -1)
        persp-auto-save-opt 0
        persp-switch-to-added-buffer nil)

  :config
  (@add-hook emacs-startup (persp-mode 1))

  ;; Ensure unreal/popup buffers aren't saved
  (push (lambda (buf) (doom-popup-p (get-buffer-window buf)))
        persp-filter-save-buffers-functions)
  (push (lambda (buf) (not (doom-real-buffer-p buf)))
        persp-common-buffer-filter-functions)

  ;; Auto-add buffers when opening them. Allows a perspective-specific buffer list.
  (defun +workspaces*auto-add-buffer (buffer &rest _)
    (when (and persp-mode (not persp-temporarily-display-buffer))
      (persp-add-buffer buffer (get-current-persp) nil)))
  (advice-add 'switch-to-buffer :after '+workspaces*auto-add-buffer)
  (advice-add 'display-buffer   :after '+workspaces*auto-add-buffer)

  ;; TODO Integration with projectile?
  ;; ;; Create a new workspace on project switch
  ;; (defun doom|new-workspace-on-project-change ()
  ;;   (+workspace-new (f-filename (doom-project-root))))
  ;; (add-hook 'projectile-before-switch-project-hook 'doom|new-workspace-on-project-change)

  ;; TODO Test per-frame perspectives

  ;; We use this instead of persp's native autosave. Why? So the "Wrote
  ;; .../_autosave" message appears AFTER the quit confirmation prompt!
  ;;
  ;; ...I need help.
  (defun +workspace|save-on-quit ()
    (when persp-mode (@quiet (persp-save-state-to-file))))
  (add-hook 'kill-emacs-hook '+workspace|save-on-quit)

  (defun +workspaces*reinit-popups (&rest _)
    (run-hook-with-args '+workspaces-load-session-hook (window-list)))
  (advice-add 'persp-load-state-from-file :after '+workspaces*reinit-popups)

  ;; Restore popups on load
  (defun +workspaces|restore-popups (windows)
    (dolist (window windows)
      (let ((plist (window-parameter window 'popup)))
        (when plist (doom-popup--init window plist)))))
  (add-hook '+workspaces-load-session-hook '+workspaces|restore-popups))

(@after ivy
  (defun +workspaces|ivy-ignore-non-persp-buffers (b)
    (when persp-mode
      (let ((persp (get-current-persp)))
        (and persp (not (persp-contain-buffer-p b persp))))))
  (pushnew '+workspaces|ivy-ignore-non-persp-buffers ivy-ignore-buffers)

  (setq ivy-sort-functions-alist
        (append ivy-sort-functions-alist
                '((persp-kill-buffer   . nil)
                  (persp-remove-buffer . nil)
                  (persp-add-buffer    . nil)
                  (persp-switch        . nil)
                  (persp-window-switch . nil)
                  (persp-frame-switch  . nil)
                  (+workspace/switch-to . nil)
                  (+workspace/delete . nil)))))


