;;; feature/workspaces/config.el

;; `persp-mode' gives me workspaces, a workspace-restricted `buffer-list', and
;; file-based session persistence. The switch from workgroups2 was motivated by
;; performance. Workgroups2 wasn't entirely stable either.

(@use-package persp-mode :demand t
  :init
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-name "main"
        persp-auto-save-fname "_autosave"
        persp-save-dir (concat doom-cache-dir "workspaces/")
        persp-set-last-persp-for-new-frames nil
        persp-auto-resume-time (if (display-graphic-p) 0.01 -1)
        persp-switch-to-added-buffer nil)

  :config
  (@add-hook emacs-startup (persp-mode 1))

  ;; Ensure unreal/popup buffers aren't saved
  (push (lambda (buf) (doom-popup-p (get-buffer-window buf)))
        persp-filter-save-buffers-functions)
  (push (lambda (buf) (not (doom-real-buffer-p buf)))
        persp-common-buffer-filter-functions)

  ;; Auto-add buffers when opening them. Allows a perspective-specific buffer list.
  (defun doom*persp-auto-add-buffer (buffer &rest _)
    (when (and persp-mode (not persp-temporarily-display-buffer))
      (persp-add-buffer buffer (get-current-persp) nil)))
  (advice-add 'switch-to-buffer :after 'doom*persp-auto-add-buffer)
  (advice-add 'display-buffer   :after 'doom*persp-auto-add-buffer)

  ;; TODO Integration with projectile
  ;; ;; Create a new workspace on project switch
  ;; (defun doom|new-workspace-on-project-change ()
  ;;   (+workspace-new (f-filename (doom-project-root))))
  ;; (add-hook 'projectile-before-switch-project-hook 'doom|new-workspace-on-project-change)

  ;; TODO Test per-frame perspectives

  ;; Restore popups on load
  (defun +workspaces*reinit-popups ()
    (dolist (window (window-list))
      (let ((plist (window-parameter window 'popup)))
        (when plist (doom-popup--init window plist)))))
  (advice-add 'persp-load-state-from-file :after '+workspaces*reinit-popups))

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


