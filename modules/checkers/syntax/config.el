;;; checkers/syntax/config.el -*- lexical-binding: t; -*-

;;
;;; Flycheck

(use-package! flycheck
  :unless (modulep! +flymake)
  :commands flycheck-list-errors flycheck-buffer
  :hook (doom-first-buffer . global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don't recheck on idle as often
  (setq flycheck-idle-change-delay 1.0)

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25)

  ;; HACK: Protect against eager expansion of `setf'. The gv setter won't be
  ;;   available until after `flycheck' loads, but macro expand occurs when this
  ;;   file is loaded.
  (eval '(setf (flycheck-checker-get 'emacs-lisp 'predicate)
               (lambda ()
                 (and
                  ;; Do not check buffers that ask not to be byte-compiled.
                  (not (bound-and-true-p no-byte-compile))
                  ;; Disable the emacs-lisp checker in non-project (likely
                  ;; untrusted) buffers to mitigate potential code execution
                  ;; vulnerability during macro expansion. See CVE-2024-53920.
                  (doom-project-p))))
        t)

  ;; Don't commandeer input focus if the error message pops up (happens when
  ;; tooltips and childframes are disabled).
  (set-popup-rules!
    '(("^\\*Flycheck error messages\\*" :select nil)
      ("^\\*Flycheck errors\\*" :size 0.25)))

  (add-hook! 'doom-escape-hook :append
    (defun +syntax-check-buffer-h ()
      "Flycheck buffer on ESC in normal mode."
      (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil)))

  (map! :map flycheck-error-list-mode-map
        :n "C-n"    #'flycheck-error-list-next-error
        :n "C-p"    #'flycheck-error-list-previous-error
        :n "j"      #'flycheck-error-list-next-error
        :n "k"      #'flycheck-error-list-previous-error
        :n "RET"    #'flycheck-error-list-goto-error
        :n [return] #'flycheck-error-list-goto-error))


(use-package! flycheck-popup-tip
  :unless (modulep! +flymake)
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :hook (flycheck-mode . +syntax-init-popups-h)
  :config
  (setq flycheck-popup-tip-error-prefix (if (modulep! +icons) "⚠ " "[!] "))

  ;; HACK: Only display the flycheck popup if we're in normal mode (for evil
  ;;   users) or if no selection or completion is active. This popup can
  ;;   interfere with the active evil mode, clear active regions, and other
  ;;   funny business (see #7242).
  (defadvice! +syntax--disable-flycheck-popup-tip-maybe-a (&rest _)
    :before-while #'flycheck-popup-tip-show-popup
    (if (and (bound-and-true-p evil-local-mode)
             (not (evil-emacs-state-p)))
        (evil-normal-state-p)
      (and (not (region-active-p))
           (not (bound-and-true-p company-backend))
           (not (ignore-errors (>= corfu--index 0)))))))


(use-package! flycheck-posframe
  :when (modulep! +childframe)
  :unless (modulep! +flymake)
  :hook (flycheck-mode . +syntax-init-popups-h)
  :config
  (if (modulep! +icons)
      (setq flycheck-posframe-warning-prefix "⚠ "
            flycheck-posframe-info-prefix "ⓘ "
            flycheck-posframe-error-prefix "⮾ ")
    (setq flycheck-posframe-warning-prefix "[?] "
          flycheck-posframe-info-prefix "[i] "
          flycheck-posframe-error-prefix "[!] "))

  ;; HACK: Hide the flycheck posframe immediately on the next keypress/user
  ;;   action, otherwise it lingers until the next time the user is idle.
  (defun +syntax--flycheck-posframe-hide-h ()
    (unless (flycheck-posframe-check-position)
      (posframe-hide flycheck-posframe-buffer))
    (remove-hook 'post-command-hook #'+syntax--flycheck-posframe-hide-h))

  (defadvice! +syntax-hide-posframe-on-next-command-a (fn &rest args)
    :around #'flycheck-posframe-show-posframe
    (letf! ((defun posframe-show (&rest args)
              (add-hook 'post-command-hook #'+syntax--flycheck-posframe-hide-h)
              (apply posframe-show args)))
      (apply fn args)))

  (after! company
    ;; Don't display popups if company is open
    (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p))
  (after! evil
    ;; Don't display popups while in insert or replace mode, as it can affect
    ;; the cursor's position or cause disruptive input delays.
    (add-hook! 'flycheck-posframe-inhibit-functions
               #'evil-insert-state-p
               #'evil-replace-state-p)))


;;
;;; Flymake

(use-package! flymake
  :when (modulep! +flymake)
  :hook ((prog-mode text-mode) . flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe)

  ;; HACK: Disable the emacs-lisp checker in non-project (likely untrusted)
  ;;   buffers to mitigate potential code execution vulnerability during macro
  ;;   expansion. See CVE-2024-53920.
  (defadvice! +syntax--only-check-elisp-buffers-in-projects-a (fn &rest args)
    "Prevent the elisp checker in non-project buffers (for CVE-2024-53920)."
    :before-while #'elisp-flymake-byte-compile
    (doom-project-p)))


(use-package! flymake-popon
  :when (modulep! +flymake)
  :hook (flymake-mode . flymake-popon-mode)
  :config
  (setq flymake-popon-method (if (modulep! +childframe)
                                 'posframe
                               'popon)))
