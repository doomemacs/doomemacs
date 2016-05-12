;;; macros-evil.el

;;;###autoload
(defmacro def-textobj! (key start-regex end-regex)
  (let ((inner-name (make-symbol "narf--inner-name"))
        (outer-name (make-symbol "narf--outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;;;###autoload
(defmacro def-tmp-excmd! (cmd-on cmd-off &rest commands)
  "Creates a toggle for a set of ex commands, named CMD-ON and CMD-OFF."
  (declare (indent 2))
  `(progn
     (defun ,cmd-on (&rest _)
       (mapc (lambda (cmd) (evil-ex-define-cmd (car cmd) (cdr cmd)))
             ',commands))
     (defun ,cmd-off (&rest _)
       (mapc (lambda (cmd) (narf/evil-ex-undefine-cmd (car cmd)))
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
