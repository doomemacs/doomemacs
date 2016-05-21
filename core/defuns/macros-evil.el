;;; macros-evil.el

;;;###autoload
(defmacro def-tmp-excmd! (cmd-on cmd-off &rest commands)
  "Creates a toggle for a set of ex commands, named CMD-ON and CMD-OFF."
  (declare (indent 2))
  `(progn
     (defun ,cmd-on (&rest _)
       (mapc (lambda (cmd) (evil-ex-define-cmd (car cmd) (cdr cmd)))
             ',commands))
     (defun ,cmd-off (&rest _)
       (mapc (lambda (cmd) (doom/evil-ex-undefine-cmd (car cmd)))
             ',commands))))

;; Shortcuts for the evil expression register
;;;###autoload
(defmacro $= (str &rest args) `(calc-eval (format ,str ,@args)))

;;;###autoload
(defmacro $r (char) `(evil-get-register ,char))

;;;###autoload
(defmacro $expand (path) `(evil-ex-replace-special-filenames ,path))

(provide 'macros-evil)
;;; macros-evil.el ends here
