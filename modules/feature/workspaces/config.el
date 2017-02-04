;;; feature/workspaces/config.el

(defvar +workspace-frames '()
  "A list of all the frames opened as separate workgroups. See
defuns/defuns-workgroups.el.")

(defvar +workspace-names '()
  "Keeps track of the fixed names for workgroups (set with :tabrename), so that
these workgroups won't be auto-renamed.")

(defvar +workspaces-dir (concat doom-cache-dir "workgroups/")
  "Path to workspaces.")


(use-package! workgroups2 :demand t
  :init
  (setq wg-workgroup-directory +workspaces-dir
        wg-session-file (concat wg-workgroup-directory "last")
        wg-first-wg-name "*untitled*"
        wg-session-load-on-start nil
        wg-mode-line-display-on nil
        wg-mess-with-buffer-list nil
        wg-emacs-exit-save-behavior 'save ; Options: 'save 'ask nil
        wg-workgroups-mode-exit-save-behavior 'save
        wg-log-level 0

        ;; NOTE: Some of these make workgroup-restoration unstable
        wg-restore-mark t
        wg-restore-frame-position t
        wg-restore-remote-buffers nil
        wg-restore-scroll-bars nil
        wg-restore-fringes nil
        wg-restore-margins nil
        wg-restore-point-max t ; Throws silent errors if non-nil

        wg-list-display-decor-divider " "
        wg-list-display-decor-left-brace ""
        wg-list-display-decor-right-brace "| "
        wg-list-display-decor-current-left ""
        wg-list-display-decor-current-right ""
        wg-list-display-decor-previous-left ""
        wg-list-display-decor-previous-right "")

  :config
  (eval-when-compile
    (unless (f-exists-p +workspaces-dir)
      (make-directory +workspaces-dir t)))

  ;; Remember fixed workgroup names between sessions
  (push '+workspace-names savehist-additional-variables)

  ;; `wg-mode-line-display-on' wasn't enough
  (advice-add 'wg-change-modeline :override 'ignore)
  ;; Don't remember popup and neotree windows
  (add-hook 'kill-emacs-hook '+workspace-cleanup)

  (after! projectile
    ;; Create a new workspace on project switch
    ;; FIXME Possibly bug?
    (defun doom*workspace-projectile-switch-project ()
      (let ((project-root (doom-project-root)))
        (doom:workspace-new nil (file-name-nondirectory (directory-file-name project-root)) t)
        (doom-reload-scratch-buffer project-root)
        (when (featurep 'neotree)
          (neotree-projectile-action))))
    (setq projectile-switch-project-action 'doom*workspace-projectile-switch-project))

  (workgroups-mode +1)
  ;; Ensure there is always a workgroup active
  (wg-create-workgroup wg-first-wg-name))

