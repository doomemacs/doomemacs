;;; core/autoload/help.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/describe-setting (setting)
  "Open the documentation of SETTING (a keyword defined with `def-setting!')."
  (interactive
   ;; TODO try to read setting from whole line
   (let ((keyword (thing-at-point 'symbol t)))
     (list (completing-read
            (format "Describe setting%s: "
                    (if (equal (substring keyword 0 1) ":")
                        (format " (default %s)" keyword)
                      ""))
            doom-settings
            nil t nil nil keyword))))
  (let ((fn (intern-soft (format "doom-setting--setter%s" setting))))
    (unless fn
      (error "'%s' is not a valid DOOM setting" setting))
    (describe-function fn)))

;;;###autoload
(defun doom/describe-module (module)
  "Open the documentation of MODULE (a string that represents the category and
submodule in the format, e.g. ':feature evil')."
  (interactive
   ;; TODO try to read module from whole line
   (list (completing-read "Describe module: "
                          (mapcar (lambda (x) (format "%s %s" (car x) (cdr x)))
                                  (reverse (hash-table-keys doom-modules)))
                          nil t)))
  (destructuring-bind (category submodule) (mapcar #'intern (split-string module " "))
    (unless (member (cons category submodule) (doom--module-pairs))
      (error "'%s' isn't a valid module" module))
    (let ((doc-path (expand-file-name "README.org" (doom-module-path category submodule))))
      (unless (file-exists-p doc-path)
        (error "There is no documentation for this module"))
      (find-file doc-path))))
