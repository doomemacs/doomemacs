;;; app/email/autoload.el

;;;###autoload
(defun =email ()
  (interactive)
  (call-interactively 'mu4e))

;;;###autoload
(defun +email/compose ()
  (interactive)
  (call-interactively 'mu4e-compose-new))

;;;###autoload
(defun +email/mark-multiple (beg end)
  "Mark all messages within the current selection in mu4e's header view. Uses
`this-command-keys' to see what flag you mean."
  (interactive "r")
  (let ((command (lookup-key mu4e-headers-mode-map (this-command-keys)))
        (lines (count-lines beg end)))
    (unless command
      (error "No valid command"))
    (when (featurep 'evil)
      (evil-normal-state))
    (save-excursion
      (goto-char beg)
      (dotimes (_ lines)
        (call-interactively command)))))
