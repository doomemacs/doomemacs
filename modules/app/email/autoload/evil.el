;;; app/email/autoload/evil.el

;;;###autoload
(defun +email/mark (&optional beg end)
  "Mark all messages within the current selection in mu4e's header view. Uses
`this-command-keys' to see what flag you mean."
  (interactive)
  (let* ((beg (or (and (region-active-p) evil-visual-beginning) (line-beginning-position)))
         (end (or (and (region-active-p) evil-visual-end) (line-end-position)))
         (key (this-command-keys))
         (command (car (cl-find-if (lambda (mark) (equal (car (plist-get (cdr mark) :char)) key))
                                   mu4e-marks))))
    (unless command
      (error "Not a valid mark command: %s" key))
    (when (featurep 'evil)
      (evil-normal-state))
    (goto-char beg)
    (dotimes (_ (count-lines beg end))
      (mu4e-headers-mark-and-next command))))
