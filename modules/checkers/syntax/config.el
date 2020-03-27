;;; checkers/syntax/config.el -*- lexical-binding: t; -*-

;;
;;; Flycheck

(use-package! flycheck
  :commands flycheck-list-errors flycheck-buffer
  :after-call doom-switch-buffer-hook after-find-file
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Check only when saving or opening files. Newline & idle checks are a mote
  ;; excessive and can catch code in an incomplete state, producing false
  ;; positives, so we removed them.
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25)

  ;; Don't commandeer input focus if the error message pops up (happens when
  ;; tooltips and childframes are disabled).
  (set-popup-rule! "^\\*Flycheck error messages\\*" :select nil)

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
        :n [return] #'flycheck-error-list-goto-error)

  (global-flycheck-mode +1))


(use-package! flycheck-popup-tip
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :hook (flycheck-mode . +syntax-init-popups-h)
  :config
  (setq flycheck-popup-tip-error-prefix "✕ ")
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
  :when (featurep! +childframe)
  :hook (flycheck-mode . +syntax-init-popups-h)
  :config
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ ")
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
;;; TODO Flymake
