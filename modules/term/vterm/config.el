;;; term/vterm/config.el -*- lexical-binding: t; -*-

(use-package! vterm
  :when (bound-and-true-p module-file-suffix)
  :commands vterm-mode
  :hook (vterm-mode . doom-mark-buffer-as-real-h)
  :hook (vterm-mode . hide-mode-line-mode) ; modeline serves no purpose in vterm
  :config
  (set-popup-rule! "^vterm" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)

  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t)

  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000)

  (setq-hook! 'vterm-mode-hook
    ;; Don't prompt about dying processes when killing vterm
    confirm-kill-processes nil
    ;; Prevent premature horizontal scrolling
    hscroll-margin 0)

  ;; Restore the point's location when leaving and re-entering insert mode.
  ; (when (featurep! :editor evil)
  ;   (add-hook! 'vterm-mode-hook
  ;     (defun +vterm-init-remember-point-h ()
  ;       (add-hook 'evil-insert-state-exit-hook #'+vterm-remember-insert-point-h nil t)
  ;       (add-hook 'evil-insert-state-entry-hook #'+vterm-goto-insert-point-h nil t)))))

  (when (featurep! :editor evil)
      (defun vterm-evil-insert ()
        (interactive)
        (vterm-goto-char (point))
        (call-interactively #'evil-insert))

      (defun vterm-evil-append ()
        (interactive)
        (vterm-goto-char (1+ (point)))
        (call-interactively #'evil-append))

      (defun vterm-evil-delete ()
        "Provide similar behavior as `evil-delete'."
        (interactive)
        (let ((inhibit-read-only t)
              )
          (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
            (call-interactively 'evil-delete))))

      (defun vterm-evil-delete-line ()
        "Provide similar behavior as `evil-delete-line'."
        (interactive)
        (let ((inhibit-read-only t)
              )
          (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
            (call-interactively 'evil-delete-line))))

      (defun vterm-evil-delete-char ()
        "Provide similar behavior as `evil-delete-char'."
        (interactive)
        (let ((inhibit-read-only t)
              )
          (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
            (call-interactively 'evil-delete-char))))

      (defun vterm-evil-change ()
        "Provide similar behavior as `evil-change'."
        (interactive)
        (let ((inhibit-read-only t))
          (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
            (call-interactively 'evil-change))))

    (add-hook! 'vterm-mode-hook
      (evil-local-mode +1)
      (evil-define-key 'normal 'local "i" 'vterm-evil-insert)
      (evil-define-key 'normal 'local "a" 'vterm-evil-append)
      (evil-define-key 'normal 'local "d" 'vterm-evil-delete)
      (evil-define-key 'normal 'local "D" 'vterm-evil-delete-line)
      (evil-define-key 'normal 'local "x" 'vterm-evil-delete-char)
      (evil-define-key 'normal 'local "c" 'vterm-evil-change))))
