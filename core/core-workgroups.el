;;; core-workgroups.el
;; see lib/workgroup-defuns.el

(use-package workgroups2
  :when (display-graphic-p)
  :init
  (setq-default
   wg-session-file          (expand-file-name "wg-default" narf-temp-dir)
   wg-workgroup-directory   (expand-file-name "workgroups" narf-temp-dir)
   wg-first-wg-name         "*untitled*"
   wg-session-load-on-start t
   wg-mode-line-display-on  nil
   wg-mess-with-buffer-list nil
   wg-emacs-exit-save-behavior           'save ; Options: 'save 'ask nil
   wg-workgroups-mode-exit-save-behavior 'save
   wg-log-level 0

   wg-list-display-decor-divider         " "
   wg-list-display-decor-left-brace      ""
   wg-list-display-decor-right-brace     "| "
   wg-list-display-decor-current-left    ""
   wg-list-display-decor-current-right   ""
   wg-list-display-decor-previous-left   ""
   wg-list-display-decor-previous-right  "")
  :config
  ;; Don't mess with the modeline!
  (advice-add 'wg-change-modeline :override 'ignore)

  (defvar narf/helm-source-wg
    '((name       . "Workgroups")
      (candidates . wg-workgroup-names)
      (action     . narf/wg-helm-switch-to-workgroup)))

  (add-to-list 'savehist-additional-variables 'narf-wg-names)
  (defvar narf-wg-frames '())
  (defvar narf-wg-names '())

  (unless (file-exists-p wg-workgroup-directory)
    (mkdir wg-workgroup-directory))

  (after! projectile
    ;; Create a new workgroup on switch-project
    (setq projectile-switch-project-action 'narf/wg-projectile-switch-project))

  ;; Don't remember popwin windows
  (add-hook! (kill-emacs wg-before-switch-to-workgroup) 'popwin:close-popup-window)

  ;; Initialize!
  (add-hook! after-init 'workgroups-mode))

(provide 'core-workgroups)
;;; core-workgroups.el ends here
