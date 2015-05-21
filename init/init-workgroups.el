(use-package workgroups2
  :config
  (progn
    (setq wg-session-file "~/.emacs.workgroup")
    (setq wg-workgroup-directory "~/.emacs.d/workgroups/")
    (setq wg-first-wg-name "main")

    (setq wg-session-load-on-start t)

    ;; What to do on Emacs exit / workgroups-mode exit?
    (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
    (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil

    (evil-define-command my:save-session (&optional bang session-name)
      (interactive "<!><a>")
      (if session-name
          (wg-save-session-as (concat wg-workgroup-directory session-name) (not bang))
        (wg-save-session)))

    (evil-define-command my:load-session (&optional bang session-name)
      (interactive "<!><a>")
      (wg-open-session (if session-name
                           (concat wg-workgroup-directory session-name)
                         wg-session-file)))

    (evil-define-command my:new-workgroup (bang name)
      (interactive "<!><a>")
      (unless name
        (user-error "No name specified for new workgroup"))
      (if bang
          (wg-clone-workgroup (wg-current-workgroup) name)
        (wg-create-workgroup name t)))

    (evil-define-command my:rename-workgroup (new-name)
      (interactive "<a>")
      (wg-rename-workgroup new-name))

    (after "helm"
      (defun my-wg-switch-to-workgroup (name)
        (wg-switch-to-workgroup (wg-get-workgroup name)))

      (defun helm-wg ()
        (interactive)
        (helm :sources '(helm-source-wg)))

      (defvar helm-source-wg
            '((name . "Workgroups")
              (candidates . wg-workgroup-names)
              (action . my-wg-switch-to-workgroup))))

    ;; Turns projectile switch-project interface (or helm's interface to it)
    ;; create a new workgroup for the new project.
    (after "projectile"
      (defun my-projectile-workgroup-switch-project ()
        (let ((workgroup-name (file-name-nondirectory (directory-file-name (project-root)))))
          (wg-create-workgroup workgroup-name t)
          (helm-projectile-find-file)))
      (setq projectile-switch-project-action 'my-projectile-workgroup-switch-project))

    (workgroups-mode 1)))


(provide 'init-workgroups)
;;; init-workgroups.el ends here
