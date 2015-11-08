;;; core-workgroups.el
;; see lib/workgroup-defuns.el

(use-package workgroups2
  :when window-system
  :init
  (setq split-height-threshold 15

        wg-session-file          (expand-file-name "wg-default" narf-temp-dir)
        wg-workgroup-directory   (expand-file-name "workgroups" narf-temp-dir)
        wg-first-wg-name         "main"
        wg-session-load-on-start t
        wg-mode-line-display-on  nil
        wg-mess-with-buffer-list t
        ;; What to do on Emacs exit / workgroups-mode exit?
        wg-emacs-exit-save-behavior           'save       ; Options: 'save 'ask nil
        wg-workgroups-mode-exit-save-behavior 'save
        wg-log-level 0

        wg-list-display-decor-divider         " : "
        wg-list-display-decor-current-left    "["
        wg-list-display-decor-current-right   "]"
        wg-list-display-decor-previous-left   ""
        wg-list-display-decor-previous-right  "")
  :config
  (defvar narf/helm-source-wg
    '((name       . "Workgroups")
      (candidates . wg-workgroup-names)
      (action     . narf/wg-helm-switch-to-workgroup)))

  (defvar narf-wg-frames '())

  (after! projectile
    ;; Turns projectile switch-project interface (or helm's interface to it)
    ;; create a new workgroup for the new project.
    (setq projectile-switch-project-action 'narf/wg-projectile-switch-project))

  ;; Initialize!
  (add-hook! after-init
    (workgroups-mode 1)
    (diminish 'workgroups-mode)))

(provide 'core-workgroups)
;;; core-workgroups.el ends here
