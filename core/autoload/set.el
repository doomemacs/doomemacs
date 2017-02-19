;;; set.el
(provide 'doom-lib-set)

;; This provides a centralized configuration system that a) won't evaluate its
;; arguments if it doesn't need to (performance), b) won't complain if the
;; setting doesn't exist and c) is more elegant than a bunch of `@after' blocks,
;; which can cause intermittent stuttering in large quantities. I'm a fan of
;; concise, do-what-I-mean front-facing configuration, believe it or not.
;;
;; Plus, it can benefit from byte-compilation.

;;;###autoload
(defmacro @def-setting (keyword arglist &optional docstring &rest forms)
  "Define a setting macro. Like `defmacro', this should return a form to be
executed when called with `@set'. FORMS are not evaluated until `@set' calls it."
  (declare (indent defun) (doc-string 3))
  (unless (keywordp keyword)
    (error "Not a valid property name: %s" keyword))
  `(defun ,(intern (format "doom-setting--setter%s" keyword)) ,arglist
     ,docstring
     ,@forms))

;;;###autoload
(defmacro @set (keyword &rest values)
  "Set an option defined by `@def-setting'. Skip if doesn't exist."
  (declare (indent defun))
  (unless values
    (error "Empty @set for %s" keyword))
  (let ((fn (intern (format "doom-setting--setter%s" keyword))))
    (if (functionp fn)
        (apply fn (eval `(list ,@values)))
      (when doom-debug-mode
        (message "No setting found for %s" keyword)))))


;; (defun describe-setting ()
;;   (interactive)
;;   ;; TODO
;;   (error "Not implemented yet"))

