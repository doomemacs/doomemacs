;;; macros-eval.el

;;;###autoload
(defmacro def-builder! (mode command &optional build-file)
  "Register major/minor MODE with build COMMAND. If FILES are provided, do an additional
check to make sure they exist in the project root."
  (let ((fn (intern (format "doom--init-builder-%s" mode))))
    `(progn
       (defun ,fn ()
         (when (or (null ,build-file)
                   (doom/project-has-files ,build-file))
           (setq doom--build-command '(,command . ,build-file))))
       ,(when (eq major-mode mode)
          (funcall fn))
       (add-hook! ,mode ',fn))))

;;;###autoload
(defmacro def-repl! (mode command)
  "Define a REPL for a mode."
  `(push '(,mode . ,command) rtog/mode-repl-alist))

(provide 'macros-eval)
;;; macros-eval.el ends here
