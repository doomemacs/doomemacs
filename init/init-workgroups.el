(use-package workgroups2
  :init
  (setq wg-session-file          (expand-file-name "wg-default" TMP-DIR)
        wg-workgroup-directory   (expand-file-name "workgroups" TMP-DIR)
        wg-first-wg-name         "main"
        wg-session-load-on-start t
        wg-mode-line-display-on  nil
        ;; What to do on Emacs exit / workgroups-mode exit?
        wg-emacs-exit-save-behavior           'save       ; Options: 'save 'ask nil
        wg-workgroups-mode-exit-save-behavior 'save)
  :config
  (progn
    (after "helm"
      (defun narf/helm-switch-to-workgroup (name)
        (wg-switch-to-workgroup (wg-get-workgroup name)))
      (defvar narf/helm-source-wg
            '((name       . "Workgroups")
              (candidates . wg-workgroup-names)
              (action     . narf/helm-switch-to-workgroup)))
      (defun narf:helm-wg ()
        (interactive)
        (helm :sources '(helm-source-wg))))

    ;; Turns projectile switch-project interface (or helm's interface to it)
    ;; create a new workgroup for the new project.
    (after "projectile"
      (defun narf/wg-projectile-switch-project ()
        (let ((workgroup-name (file-name-nondirectory (directory-file-name (narf/project-root)))))
          (wg-create-workgroup workgroup-name t)
          (helm-projectile-find-file)))
      (setq projectile-switch-project-action 'narf/wg-projectile-switch-project))

    ;; Initialize!
    (defun narf|init-workgroups ()
      (workgroups-mode +1)
      (diminish 'workgroups-mode))
    (add-hook 'after-init-hook 'narf|init-workgroups)))


(provide 'init-workgroups)
;;; init-workgroups.el ends here
