;;; core-sessions.el

;; I use workgroups to accomplish two things:
;;   1. Vim-like tab emulation (type :tabs to see a list of tabs -- maybe I'll add some
;;      code to make a permanent frame header to display these some day)
;;   2. Session persistence

(use-package workgroups2
  :when (display-graphic-p)
  :init
  (setq-default
   wg-session-file          (expand-file-name "workgroups/last" narf-temp-dir)
   wg-workgroup-directory   (expand-file-name "workgroups/" narf-temp-dir)
   wg-first-wg-name         "*untitled*"
   wg-session-load-on-start nil
   wg-mode-line-display-on  nil
   wg-mess-with-buffer-list nil
   wg-emacs-exit-save-behavior           'save ; Options: 'save 'ask nil
   wg-workgroups-mode-exit-save-behavior 'save
   wg-log-level 0

   ;; NOTE: Some of these make workgroup-restoration unstable
   wg-restore-mark t
   wg-restore-frame-position nil
   wg-restore-remote-buffers nil
   wg-restore-scroll-bars nil
   wg-restore-fringes nil
   wg-restore-margins nil
   wg-restore-point-max t ; Throws silent errors if non-nil

   wg-list-display-decor-divider         " "
   wg-list-display-decor-left-brace      ""
   wg-list-display-decor-right-brace     "| "
   wg-list-display-decor-current-left    ""
   wg-list-display-decor-current-right   ""
   wg-list-display-decor-previous-left   ""
   wg-list-display-decor-previous-right  "")

  (add-hook! emacs-startup (workgroups-mode +1))
  :config
  (unless (file-exists-p wg-workgroup-directory)
    (mkdir wg-workgroup-directory))

  (defvar narf-wg-frames '()
    "A list of all the frames opened as separate workgroups. See
defuns/defuns-workgroups.el.")
  (defvar narf-wg-names '()
    "A list of fixed names for workgroups. If a name is set, workgroup names aren't
automatically renamed to the project name.")

  ;; Remember the set names in between sessions
  (add-to-list 'savehist-additional-variables 'narf-wg-names)

  (after! projectile
    ;; Create a new workgroup on switch-project
    (setq projectile-switch-project-action 'narf/wg-projectile-switch-project))

  ;; Seriously, don't mess with the modeline! `wg-mode-line-display-on' isn't enough
  (advice-add 'wg-change-modeline :override 'ignore)

  ;; Don't remember popup and neotree windows
  (add-hook! kill-emacs
    (narf/popup-close-all)
    (when (and (featurep 'neotree) (neo-global--window-exists-p))
      (neotree-hide)))

  ;; This helps abstract some of the underlying functions away, just in case I want to
  ;; switch to a different package in the future, like persp-mode, eyebrowse or wconf.
  (defalias 'narf/tab-display 'narf/workgroup-display)
  (defalias 'narf/helm-tabs 'narf:helm-wg)
  (defalias 'narf/close-window-or-tab 'narf/close-window-or-workgroup)
  (defalias 'narf:tab-create 'narf:workgroup-new)
  (defalias 'narf:tab-rename 'narf:workgroup-rename)
  (defalias 'narf:kill-tab 'narf:workgroup-delete)
  (defalias 'narf:kill-other-tabs  'narf:kill-other-workgroups)
  (defalias 'narf:switch-to-tab 'narf:switch-to-workgroup-at-index)
  (defalias 'narf:switch-to-tab-left 'wg-switch-to-workgroup-left)
  (defalias 'narf:switch-to-tab-right 'wg-switch-to-workgroup-right)
  (defalias 'narf:switch-to-tab-last 'wg-switch-to-previous-workgroup))

(provide 'core-sessions)
;;; core-sessions.el ends here
