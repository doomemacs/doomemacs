;;; tools/lookup/autoload/docsets.el -*- lexical-binding: t; -*-
;;;###if (featurep! +docsets)

(defvar dash-docs-docsets nil)

;;;###autodef
(defun set-docsets! (modes &rest docsets)
  "Registers a list of DOCSETS for MODES.

MODES can be one major mode, or a list thereof.

DOCSETS can be strings, each representing a dash docset, or a vector with the
structure [DOCSET FORM]. If FORM evaluates to nil, the DOCSET is omitted. If it
is non-nil, (format DOCSET FORM) is used as the docset.

The first element in DOCSETS can be :add or :remove, making it easy for users to
add to or remove default docsets from modes.

DOCSETS can also contain sublists, which will be flattened.

Example:

  (set-docsets! '(js2-mode rjsx-mode) \"JavaScript\"
    [\"React\" (eq major-mode 'rjsx-mode)]
    [\"TypeScript\" (bound-and-true-p tide-mode)])

Used by `+lookup/in-docsets' and `+lookup/documentation'."
  (declare (indent defun))
  (let ((action (if (keywordp (car docsets)) (pop docsets))))
    (dolist (mode (doom-enlist modes))
      (let ((hook (intern (format "%s-hook" mode)))
            (fn (intern (format "+lookup|init--%s-%s" (or action "set") mode))))
        (if (null docsets)
            (remove-hook hook fn)
          (fset fn
                (lambda ()
                  (make-local-variable 'dash-docs-docsets)
                  (unless (memq action '(:add :remove))
                    (setq dash-docs-docset nil))
                  (dolist (spec docsets)
                    (cl-destructuring-bind (docset . pred)
                        (cl-typecase spec
                          (string (cons spec nil))
                          (vector (cons (aref spec 0) (aref spec 1)))
                          (otherwise (signal 'wrong-type-arguments (list spec '(vector string)))))
                      (when (or (null pred)
                                (eval pred t))
                        (if (eq action :remove)
                            (setq dash-docs-docsets (delete docset dash-docs-docsets))
                          (cl-pushnew docset dash-docs-docsets)))))))
          (add-hook hook fn 'append))))))

;;;###autoload
(defun +lookup-dash-docsets-backend-fn (identifier)
  "Looks up IDENTIFIER in available Dash docsets, if any are installed.

This backend is meant for `+lookup-documentation-functions'.

Docsets must be installed with one of the following commands:

+ `dash-docs-install-docset'
+ `dash-docs-install-docset-from-file'
+ `dash-docs-install-user-docset'
+ `dash-docs-async-install-docset'
+ `dash-docs-async-install-docset-from-file'

Docsets can be searched directly via `+lookup/in-docsets'."
  (when (require 'dash-docs nil t)
    (when-let (docsets (cl-remove-if-not #'dash-docs-docset-path (dash-docs-buffer-local-docsets)))
      (+lookup/in-docsets nil identifier docsets)
      'deferred)))


;;
;;; Commands

;;;###autoload
(defun +lookup/in-docsets (arg &optional query docsets)
  "Lookup QUERY in dash DOCSETS.

QUERY is a string and docsets in an array of strings, each a name of a Dash
docset. Requires either helm or ivy.

If prefix ARG is supplied, search all installed installed docsets. They can be
installed with `dash-docs-install-docset'."
  (interactive "P")
  (require 'dash-docs)
  (let ((dash-docs-common-docsets)
        (dash-docs-browser-func +lookup-open-url-fn)
        (dash-docs-docsets
         (if arg
             (dash-docs-installed-docsets)
           (cl-remove-if-not #'dash-docs-docset-path (or docsets dash-docs-docsets))))
        (query (doom-thing-at-point-or-region query)))
    (doom-log "Searching docsets %s" dash-docs-docsets)
    (cond ((featurep! :completion helm)
           (helm-dash query))
          ((featurep! :completion ivy)
           (counsel-dash query))
          ((user-error "No dash backend is installed, enable ivy or helm.")))))

;;;###autoload
(defun +lookup/in-all-docsets (&optional query)
  "TODO"
  (interactive)
  (+lookup/in-docsets t query))
