(use-package workgroups2
  :config
  (progn
    (setq wg-session-file "~/.emacs.d/workgroups/.default")

    (setq wg-session-load-on-start t)

    ;; What to do on Emacs exit / workgroups-mode exit?
    (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
    (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil

    (setq wg-mode-line-display-on nil)          ; Default: (not (featurep 'powerline))
    (setq wg-flag-modified t)                 ; Display modified flags as well
    (setq wg-mode-line-decor-left-brace "["
          wg-mode-line-decor-right-brace "]"  ; how to surround it
          wg-mode-line-decor-divider ":")
    (setq wg-mode-line-only-name t)

    (evil-define-command my:save-session (&optional bang session-name)
      (interactive "<!><a>")
      (if session-name
          (wg-save-session-as (concat (file-name-directory wg-session-file) session-name) (not bang))
        (wg-save-session)))

    (evil-define-command my:load-session (&optional bang session-name)
      (interactive "<!><a>")
      (wg-open-session (if session-name
                           (concat (file-name-directory wg-session-file) session-name)
                         wg-session-file)))

    (evil-define-command my:rename-workgroup (new-name)
      (interactive "<a>")
      (wg-rename-workgroup new-name))

    (evil-ex-define-cmd "l[oad]"    'my:load-session)
    (evil-ex-define-cmd "s[ave]"    'my:save-session)
    (evil-ex-define-cmd "wn[ext]"    'wg-switch-to-workgroup-right)
    (evil-ex-define-cmd "wp[rev]"    'wg-switch-to-workgroup-left)
    (evil-ex-define-cmd "wre[name]"  'my:rename-workgroup)
    (evil-ex-define-cmd "k[ill]w"    'wg-kill-workgroup-and-buffers)
    (evil-ex-define-cmd "k[ill]ow"   (λ
                                      (let (workgroup (wg-get-workgroup))
                                        (dolist (w (wg-workgroup-list-or-error))
                                          (unless (eq w workgroup)
                                            (wg-kill-workgroup-and-buffers w))))))

    ;; Turns projectile switch-project interface (or helm's interface to it)
    ;; create a new workgroup for the new project.
    (after "projectile"
      (defun my-projectile-workgroup-switch-project ()
        (let ((workgroup-name (file-name-nondirectory (directory-file-name (my--project-root)))))
          (wg-create-workgroup workgroup-name t)
          (helm-projectile-find-file)))

      (setq projectile-switch-project-action 'my-projectile-workgroup-switch-project))

    (workgroups-mode 1)))


(provide 'init-workgroups)
;;; init-workgroups.el ends here
