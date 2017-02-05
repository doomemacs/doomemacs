;;; core-set.el
;;
;; This provides a centralized configuration system that a) won't evaluate its
;; arguments if it doesn't need to (performance), b) won't complain if the
;; setting doesn't exist and c) is more elegant than a bunch of `after!' blocks,
;; which can cause intermittent stuttering in large quantities. I'm a fan of
;; concise, do-what-I-mean front-facing configuration, believe it or not.
;;
;; Plus, it can benefit from byte-compilation.

(define-key help-map "\C-s" 'doom/describe-setting)

(defvar doom-settings nil
  "An alist of settings, mapping setting keywords to setter functions, which can
be a lambda or symbol.")

(defmacro def-setting! (keyword arglist &optional docstring &rest forms)
  "Define a setting macro. Takes the same arguments as `defmacro'. This should
return forms, which will be run when `set!' is used to call this setting."
  (declare (indent defun) (doc-string 3))
  (unless (keywordp keyword)
    (error "Not a valid property name: %s" keyword))
  `(push (list ,keyword
               :source ,(__FILE__)
               :docstring ,docstring
               :fn (lambda ,arglist
                     ,docstring
                     ,@forms))
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
