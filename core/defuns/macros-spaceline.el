;;; defuns-spaceline.el

;;;###autoload
(defmacro def-version-cmd! (modes command)
  "Define a COMMAND for MODE that will set `doom--env-command' when that mode is
activated, which should return the version number of the current environment. It is used
by `doom|spaceline-env-update' to display a version number in the modeline. For instance:

  (def-version-cmd! ruby-mode \"ruby --version | cut -d' ' -f2\")

This will display the ruby version in the modeline in ruby-mode buffers. It is cached the
first time."
  (add-hook! (focus-in find-file) 'doom|spaceline-env-update)
  `(add-hook! ,modes (setq doom--env-command ,command)))

(provide 'defuns-spaceline)
;;; defuns-spaceline.el ends here
