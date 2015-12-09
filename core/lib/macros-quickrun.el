;;; macros-quickrun.el

;;;###autoload
(defmacro define-builder! (mode command &optional build-file)
  "Register major/minor MODE with build COMMAND. If FILES are provided, do an
additional check to make sure they exist in the project root."
  `(add-hook! ,mode
     (when (or (null ,build-file)
               (narf/project-has-files ,build-file))
       (setq narf--build-command '(,command . ,build-file)))))

;;;###autoload
(defmacro define-repl! (mode command)
  `(push '(,mode . ,command) rtog/mode-repl-alist))

(provide 'macros-quickrun)
;;; macros-quickrun.el ends here
