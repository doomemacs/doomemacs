;;; defuns-spaceline.el

;;;###autoload
(defmacro define-env-command! (mode command)
  (add-hook! (focus-in find-file) 'narf|spaceline-env-update)
  `(add-hook ',(intern (format "%s-hook" (symbol-name mode)))
             (lambda () (setq narf--env-command ,command))))

(provide 'defuns-spaceline)
;;; defuns-spaceline.el ends here
