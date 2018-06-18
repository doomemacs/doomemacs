;;; emacs/eshell/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload
(defun +eshell|init-evil ()
  "Replace `evil-collection-eshell-next-prompt-on-insert' with
`+eshell|goto-prompt-on-insert', which ensures the point is on the prompt when
changing to insert mode."
  (dolist (hook '(evil-replace-state-entry-hook evil-insert-state-entry-hook))
    (remove-hook hook 'evil-collection-eshell-next-prompt-on-insert t)
    (add-hook hook '+eshell|goto-prompt-on-insert nil t)))

;;;###autoload (autoload '+eshell:run "emacs/eshell/autoload/evil" nil t)
(evil-define-command +eshell:run (command bang)
  "TODO"
  (interactive "<fsh><!>")
  (let ((buffer (+eshell-last-buffer))
        (command (+evil*resolve-vim-path command)))
    (cond (buffer
           (select-window (get-buffer-window buffer))
           (+eshell-run-command command buffer))
          (bang (+eshell/open nil command))
          ((+eshell/open-popup nil command)))))

;;;###autoload
(defun +eshell|goto-prompt-on-insert ()
  "Move cursor to the prompt when switching to insert mode (if point isn't
already there)."
  (when (< (point) eshell-last-output-end)
    (goto-char
     (if (memq this-command '(evil-append evil-append-line))
         (point-max)
       eshell-last-output-end))))

;;;###autoload
(defun +eshell/goto-end-of-prompt ()
  "Move cursor to the prompt when switching to insert mode (if point isn't
already there)."
  (interactive)
  (goto-char (point-max))
  (evil-append 1))

;;;###autoload (autoload '+eshell/evil-change "emacs/eshell/autoload/evil" nil t)
(evil-define-operator +eshell/evil-change (beg end type register yank-handler delete-func)
  "Like `evil-change' but will not delete/copy the prompt."
  (interactive "<R><x><y>")
  (save-restriction
    (narrow-to-region eshell-last-output-end (point-max))
    (evil-change (max beg (point-min))
                 (if (eq type 'line) (point-max) (min (or end (point-max)) (point-max)))
                 type register yank-handler delete-func)))

;;;###autoload (autoload '+eshell/evil-change-line "emacs/eshell/autoload/evil" nil t)
(evil-define-operator +eshell/evil-change-line (beg end type register yank-handler)
  "Change to end of line."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (+eshell/evil-change beg end type register yank-handler #'evil-delete-line))

;;;###autoload (autoload '+eshell/evil-delete "emacs/eshell/autoload/evil" nil t)
(evil-define-operator +eshell/evil-delete (beg end type register yank-handler)
  "Like `evil-delete' but will not delete/copy the prompt."
  (interactive "<R><x><y>")
  (save-restriction
    (narrow-to-region eshell-last-output-end (point-max))
    (evil-delete (if beg (max beg (point-min)) (point-min))
                 (if (eq type 'line) (point-max) (min (or end (point-max)) (point-max)))
                 type register yank-handler)))

;;;###autoload (autoload '+eshell/evil-delete-line "emacs/eshell/autoload/evil" nil t)
(evil-define-operator +eshell/evil-delete-line (_beg end type register yank-handler)
  "Change to end of line."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  (+eshell/evil-delete (point) end type register yank-handler))
