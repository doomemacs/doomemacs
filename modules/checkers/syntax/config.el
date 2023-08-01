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
  (setq flycheck-popup-tip-error-prefix "X ")
  (after! evil
    ;; Don't display popups while in insert or replace mode, as it can affect
    ;; the cursor's position or cause disruptive input delays.
    (add-hook! '(evil-insert-state-entry-hook evil-replace-state-entry-hook)
               #'flycheck-popup-tip-delete-popup)
    (defadvice! +syntax--disable-flycheck-popup-tip-maybe-a (&rest _)
      :before-while #'flycheck-popup-tip-show-popup
      (if evil-local-mode
          (eq evil-state 'normal)
        (not (bound-and-true-p company-backend))))))


(use-package! flycheck-posframe
  :when (modulep! +childframe)
  :unless (modulep! +flymake)
  :hook (flycheck-mode . +syntax-init-popups-h)
  :config
  (setq flycheck-posframe-warning-prefix "! "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "X ")
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
  :defer t
  :init
  ;; as flymakes fail silently there is no need to activate it on a per major mode basis
  (add-hook! (prog-mode text-mode) #'flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))


(use-package! flymake-popon
  :when (modulep! +flymake)
  :hook (flymake-mode . flymake-popon-mode)
  :config
  (setq flymake-popon-method (if (modulep! +childframe)
                                 'posframe
                               'popon)))
