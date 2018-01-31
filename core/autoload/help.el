;;; core/autoload/help.el -*- lexical-binding: t; -*-

(defun doom--find-next (pred forward-fn end-fn)
  (save-excursion
    (catch 'found
      (ignore-errors
        (while (not (funcall end-fn))
          (when-let* ((ret (funcall pred)))
            (throw 'found ret))
          (funcall forward-fn))))))

;;;###autoload
(defun doom/describe-setting (setting)
  "Open the documentation of SETTING (a keyword defined with `def-setting!').

Defaults to the "
  (interactive
   (let ((sym (symbol-at-point)))
     (list (completing-read "Describe setting: "
                            (sort (mapcar #'car doom-settings) #'string-lessp)
                            nil t (if (keywordp sym) (symbol-name sym))))))
  (let ((fn (cdr (assq (intern setting) doom-settings))))
    (unless fn
      (error "'%s' is not a valid DOOM setting" setting))
    (describe-function fn)))

;;;###autoload
(defun doom/describe-module (module)
  "Open the documentation of MODULE (a string that represents the category and
submodule in the format, e.g. ':feature evil').

Defaults to either a) the module at point (in init.el), b) the module derived
from a `featurep!' or `require!' call or c) the module that the current file is
in."
  (interactive
   (let ((module
          (cond ((and buffer-file-name
                      (string= (file-truename user-init-file)
                               (file-truename buffer-file-name)))
                 (let* ((category
                         (save-excursion
                           (goto-char (line-end-position))
                           (doom--find-next
                            (lambda ()
                              (and (not (sp-point-in-comment))
                                   (keywordp (symbol-at-point))
                                   (symbol-at-point)))
                            (lambda () (forward-symbol -1))
                            #'bobp)))
                        (module
                         (save-excursion
                           (goto-char (line-beginning-position))
                           (save-restriction
                             (narrow-to-region (point) (line-end-position))
                             (doom--find-next
                              (lambda ()
                                (and (not (sp-point-in-comment))
                                     (not (keywordp (symbol-at-point)))
                                     (sexp-at-point)))
                              #'forward-sexp
                              #'eobp)))))
                   (when (and category module)
                     (format "%s %s" category module))))
                ((save-excursion
                   (sp-beginning-of-sexp)
                   (unless (eq (char-after) ?\()
                     (backward-char))
                   (let ((sexp (sexp-at-point)))
                     (when (memq (car-safe sexp) '(featurep! require!))
                       (format "%s %s" (nth 1 sexp) (nth 2 sexp))))))
                ((and buffer-file-name
                      (file-in-directory-p buffer-file-name doom-modules-dir))
                 (let ((module (doom-module-from-path buffer-file-name)))
                   (format "%s %s" (car module) (cdr module)))))))
     (list (completing-read "Describe module: "
                            (cl-loop for (module . sub) in (reverse (hash-table-keys doom-modules))
                                     collect (format "%s %s" module sub))
                            nil t module))))
  (cl-destructuring-bind (category submodule)
      (mapcar #'intern (split-string module " "))
    (unless (member (cons category submodule) (doom-module-pairs))
      (error "'%s' isn't a valid module" module))
    (let ((doc-path (expand-file-name "README.org" (doom-module-path category submodule))))
      (unless (file-exists-p doc-path)
        (error "There is no documentation for this module"))
      (find-file doc-path))))
