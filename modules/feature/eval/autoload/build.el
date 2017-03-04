;;; feature/eval/autoload/build.el

(defvar-local +eval-last-builder nil
  "The last builder run in the current buffer.")

(defun +eval--read-builder ()
  (let ((builders (cl-remove-if-not
                   (lambda (plist)
                     (when-let (pred (plist-get (cdr plist) :when))
                       (eval pred)))
                   (cdr (assq major-mode +eval-builders)))))
    (completing-read
     "Build: "
     (mapcar 'car builders)
     nil t)))

;;;###autoload
(defun +eval/build (&optional builder)
  (interactive (list (+eval--read-builder)))
  (unless builder
    (error "No builder for this buffer"))
  (let ((desc (assq builder (assq major-mode +eval-builders))))
    (unless desc
      (error "Builder not found in registered builders"))
    (message "Running %s" builder)))

;;;###autoload
(defun +eval/rebuild (&optional builder)
  (interactive (list +eval-last-builder))
  (+eval/build +eval-last-builder))

