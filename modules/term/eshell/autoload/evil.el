;;; term/eshell/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload (autoload '+eshell:run "term/eshell/autoload/evil" nil t)
(evil-define-command +eshell:run (command bang)
  "TODO"
  (interactive "<fsh><!>")
  (let ((buffer (+eshell-last-buffer))
        (command (+evil-replace-filename-modifiers-a command)))
    (cond (buffer
           (select-window (get-buffer-window buffer))
           (+eshell-run-command command buffer))
          (bang (+eshell/open nil command))
          ((+eshell/open-popup nil command)))))

;;;###autoload
(defun +eshell-goto-prompt-on-insert-a ()
  "Move cursor to the prompt when switching to insert mode (if point isn't
already there).

  Meant to replace `evil-collection-eshell-next-prompt-on-insert'."
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
