;;; core-set.el

;; When we switch out feature modules, I don't want certain config segments to
;; casue errors. This gives a

(define-key help-map "\C-s" 'doom/describe-setting)

(defvar doom-settings nil
  "An alist of settings, mapping setting keywords to setter functions, which can
be a lambda or symbol.")

(defun doom-def-setting (keyword setter-fn &optional docstring)
  "Define a setting macro. Takes the same arguments as `defmacro'. This should
return forms, which will be run when `set!' is used to call this setting."
  (declare (indent defun))
  (unless (keywordp keyword)
    (error "Not a valid property name: %s" keyword))
  (unless (or (symbolp setter-fn)
              (functionp setter-fn))
    (error "Not a valid setting function for %s" keyword))
  (push (list keyword
              :source load-file-name
              :docstring docstring
              :fn setter-fn)
        doom-settings))

(defmacro set! (keyword &rest rest)
  "Set an option defined by `def-setting!'. Skip if doesn't exist."
  (declare (indent defun))
  (let* ((plist (cdr (assq keyword doom-settings)))
         (fn (plist-get plist :fn)))
    (when (and doom-debug-mode (not fn))
      (message "No setting found for %s" keyword))
    (when fn
      (macroexp-progn
       (mapcar (lambda (args) `(apply #',fn ',(-list args)))
               rest)))))

(provide 'core-set)
;;; core-set.el ends here
