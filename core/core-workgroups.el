;;; core-workgroups.el
;; see lib/workgroup-defuns.el

(use-package workgroups2
  :when (display-graphic-p)
  :init
  (add-hook! after-init 'workgroups-mode)
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

   ;; Some of these make workgroup-restoration-on-init unstable. I'd rather trade some
   ;; convenience for stability.
   wg-restore-fringes nil
   wg-restore-margins nil
   wg-restore-scroll-bars nil
   wg-restore-frame-position nil
   wg-restore-remote-buffers nil
   ;; wg-restore-mark nil
   ;; wg-restore-point-max nil

   wg-list-display-decor-divider         " "
   wg-list-display-decor-left-brace      ""
   wg-list-display-decor-right-brace     "| "
   wg-list-display-decor-current-left    ""
   wg-list-display-decor-current-right   ""
   wg-list-display-decor-previous-left   ""
   wg-list-display-decor-previous-right  "")
  :config
  (defvar narf/helm-source-wg
    '((name       . "Workgroups")
      (candidates . wg-workgroup-names)
      (action     . narf/wg-helm-switch-to-workgroup)))

  (unless (file-exists-p wg-workgroup-directory)
    (mkdir wg-workgroup-directory))

  (defvar narf-wg-frames '()
    "A list of all the frames opened as separate workgroups. See
lib/defuns-workgroups.el.")
  (defvar narf-wg-names '()
    "A list of fixed names for workgroups. If a name is set, workgroup names aren't
    automatically renamed to the project name.")

  ;; Remember the set names in between sessions
  (add-to-list 'savehist-additional-variables 'narf-wg-names)

  (after! projectile
    ;; Create a new workgroup on switch-project
    (setq projectile-switch-project-action 'narf/wg-projectile-switch-project))

  ;; Save the session every 10 minutes
  (run-with-timer 0 600 'narf/wg-autosave)

  ;; Don't mess with the modeline!
  (advice-add 'wg-change-modeline :override 'ignore)

  ;; Don't remember popup windows
  (add-hook! kill-emacs 'narf-popup-close-all)

  ;; This helps abstract some of the underlying functions away, just in case I want to
  ;; switch to a different package in the future, like persp-mode, eyebrowse or wconf.
  (defalias 'narf/helm-tabs 'narf:helm-wg)
  (defalias 'narf/close-window-or-tab 'narf/close-window-or-workgroup)
  (defalias 'narf:switch-to-tab 'narf:switch-to-workgroup-at-index)
  (defalias 'narf/tab-display 'narf/workgroup-display)
  (defalias 'narf:tab-create 'narf:workgroup-new)
  (defalias 'narf:tab-rename 'narf:workgroup-rename)
  (defalias 'narf:kill-tab 'narf:workgroup-delete)
  (defalias 'narf:kill-other-tabs  'narf:kill-other-workgroups)
  (defalias 'narf:switch-to-tab-left 'wg-switch-to-workgroup-left)
  (defalias 'narf:switch-to-tab-right 'wg-switch-to-workgroup-right)
  (defalias 'narf:switch-to-tab-last 'wg-switch-to-previous-workgroup))

(provide 'core-workgroups)
;;; core-workgroups.el ends here
