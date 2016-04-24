;;; defuns-spaceline.el

;;;###autoload
(defmacro def-env-command! (mode command)
  "Define a COMMAND for MODE that will set `narf--env-command' when that mode is
activated, which should return the version number of the current environment. It is used
by `narf|spaceline-env-update' to display a version number in the modeline. For instance:

  (def-env-command! ruby-mode \"ruby --version | cut -d' ' -f2\")

This will display the ruby version in the modeline in ruby-mode buffers. It is cached the
first time."
  (add-hook! (focus-in find-file) 'narf|spaceline-env-update)
  `(add-hook ',(intern (format "%s-hook" (symbol-name mode)))
             (lambda () (setq narf--env-command ,command))))

(provide 'defuns-spaceline)
;;; defuns-spaceline.el ends here
