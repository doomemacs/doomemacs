;;; core-set.el

;; When we switch out feature modules, I don't want certain config segments to
;; casue errors. This gives a

(define-key help-map "\C-s" 'doom/describe-setting)

(defvar doom-settings nil
  "An alist of settings, mapping setting keywords to setter functions, which can
be a lambda or symbol.")

(defmacro def-setting! (keyword arglist &optional docstring &rest body)
  "Define a setting macro. Takes the same arguments as `defmacro'. This should
return forms, which will be run when `set!' is used to call this setting."
  (declare (indent defun))
  (unless (keywordp keyword)
    (error "Not a valid property name: %s" name))
  (let ((sym (intern (format "doom--set%s" keyword))))
    (setq doom-settings (assq-delete-all keyword doom-settings))
    `(push (cons ,keyword
                 (defun ,sym ,arglist
                   ,docstring
                   ,@body))
           doom-settings)))

(defmacro set! (keyword &rest rest)
  "Set an option defined by `doom-define-settings'. Skip if doesn't exist."
  (declare (indent defun))
  (let ((set-fn (cdr (assq keyword doom-settings))))
    (when set-fn
      (macroexp-progn
       (mapcar (lambda (&rest args) (apply set-fn args))
               rest)))))

(provide 'core-set)
;;; core-set.el ends here
