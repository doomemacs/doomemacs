;;; defuns-text.el

;;;###autoload (autoload 'doom:insert-date "defuns-text" nil t)
(evil-define-command doom:insert-date (beg end &optional format)
  (interactive "<r><a>")
  (when (and (evil-visual-state-p) beg end)
    (kill-region beg end))
  (insert (format-time-string (or format "%x"))))

(provide 'defuns-text)
;;; defuns-text.el ends here
