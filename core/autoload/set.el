;;; set.el
(provide 'doom-lib-set)

;; This provides a centralized configuration system that a) won't evaluate its
;; arguments if it doesn't need to (performance), b) won't complain if the
;; setting doesn't exist and c) is more elegant than a bunch of `after!' blocks,
;; which can cause intermittent stuttering in large quantities. I'm a fan of
;; concise, do-what-I-mean front-facing configuration, believe it or not.
;;
;; Plus, it can benefit from byte-compilation.

;;;###autoload
(defvar doom-settings nil
  "An alist of settings, mapping setting keywords to setter functions, which can
be a lambda or symbol.")

;;;###autoload
(defmacro @def-setting (keyword arglist &optional docstring &rest forms)
  "Define a setting macro. Like `defmacro', this should return a form to be
executed when called with `@set'. FORMS are not evaluated until `@set' calls it."
  (declare (indent defun) (doc-string 3))
  (unless (keywordp keyword)
    (error "Not a valid property name: %s" keyword))
  (let ((sym (intern (format "doom-setting--setter%s" keyword))))
    (setq doom-settings (assq-delete-all keyword doom-settings))
    (prog1
        `(progn
           (defun ,sym ,arglist
             ,docstring
             ,@forms)
           (push ',(list keyword
                         :source (__FILE__)
                         :docstring docstring
                         :fn sym)
                 doom-settings))
      (let (byte-compile-warnings)
        (byte-compile sym)))))

;;;###autoload
(defmacro @set (keyword &rest values)
  "Set an option defined by `@def-setting'. Skip if doesn't exist."
  (declare (indent defun))
  (unless values
    (error "Empty @set for %s" keyword))
  (cond ((not values)
         (error "Empty @set for %s" keyword))
        ((not (assq keyword doom-settings))
         (when doom-debug-mode
           (warn "No setting found for %s" keyword)))
        (t
         (let* ((plist (cdr (assq keyword doom-settings)))
                (fn (plist-get plist :fn)))
           (if fn
               (let ((values (eval `(list ,@values))))
                 (apply fn values))
             (error "No setting function where one was expected for %s" keyword))))))

