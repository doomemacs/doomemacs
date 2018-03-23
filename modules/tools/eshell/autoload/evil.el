;;; tools/eshell/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload (autoload '+eshell:run "tools/eshell/autoload/evil" nil t)
(evil-define-command +eshell:run (command bang)
  ;; TODO Add COMMAND support
  (interactive "<fsh><!>")
  (if bang
      (+eshell/open command)
    (+eshell/open-popup command)))

;;;###autoload (autoload '+eshell/evil-change "tools/eshell/autoload/evil" nil t)
(evil-define-operator +eshell/evil-change (beg end type register yank-handler delete-func)
  "Like `evil-change' but will not delete/copy the prompt."
  (interactive "<R><x><y>")
  (let ((delete-func (or delete-func #'evil-delete))
        (nlines (1+ (evil-count-lines beg end)))
        (opoint (save-excursion
                  (goto-char beg)
                  (evil-collection-eshell-next-prompt)
                  (point))))
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step))
    (when (get-text-property beg 'read-only)
      (save-excursion
        (goto-char beg)
        (evil-collection-eshell-next-prompt)
        (setq beg (point))))
    (funcall delete-func beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (goto-char eshell-last-output-end)
      (when (get-text-property (point) 'read-only)
        (evil-collection-eshell-next-prompt))
      (evil-append 1))
     ((eq type 'block) (evil-insert 1 nlines))
     (t
      (evil-insert 1)))))

;;;###autoload (autoload '+eshell/evil-change-line "tools/eshell/autoload/evil" nil t)
(evil-define-operator +eshell/evil-change-line (beg end type register yank-handler)
  "Change to end of line."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (when (get-text-property beg 'read-only)
    (save-excursion
      (goto-char beg)
      (evil-collection-eshell-next-prompt)
      (setq beg (point))))
  (evil-change beg end type register yank-handler #'evil-delete-line))
