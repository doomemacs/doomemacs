;;; feature/eval/autoload/build.el -*- lexical-binding: t; -*-

(defvar-local +eval-last-builder nil
  "The last builder run in the current buffer.")

(defvar +eval-current-builder nil
  "The spec for the currently running builder. Available from inside builder
functions.")

(defun +eval--read-builder ()
  (when-let (builders
             (cl-remove-if-not
              (lambda (plist)
                (if-let (pred (plist-get plist :when))
                    (and (or (symbolp pred)
                             (functionp pred))
                         (funcall pred))
                  t))
              (cl-delete-duplicates
               (reverse (cdr (assq major-mode +eval-builders)))
               :key 'car)
              :key 'cdr))
    (if (= (length builders) 1)
        (car builders)
      (when-let (builder (completing-read "Build: " (mapcar #'car builders) nil t))
        (assq (intern builder) builders)))))

;;;###autoload
(defun +eval/build (builder)
  "TODO"
  (interactive
   (list (or +eval-last-builder
             (+eval--read-builder)
             (error "No builder for this buffer"))))
  (unless builder
    (error "Builder not found in registered builders"))
  (let ((name  (car builder))
        (fn (plist-get (cdr builder) :fn)))
    (message "Running %s" name)
    (if (or (functionp fn)
            (and (symbolp fn) (fboundp fn)))
        (let ((+eval-current-builder builder))
          (funcall fn))
      (error "'%s' builder is invalid" name))))

