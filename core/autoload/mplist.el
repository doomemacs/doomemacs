;;; plist.el
(provide 'doom-lib-mplist)

;; The built-in plist library won't work when the list is a malformed plist,
;; which is a plist who has multiple (or no) forms following its properties. For
;; example: (:x 5 :y 1 2 3 :z).
;;
;; This library was made for mplists, so use the standard plist-* functions for
;; regular plists.
;;
;; (let ((a '(abc :commands 1 :config 3 4 5)))
;;   (mplist! a &delete :commands :config))

;;;###autoload
(defmacro mplist! (var action &rest args)
  "A helper macro that makes dealing with doom-mplist-* functions a little more concise.

Examples:
  (mplist! plist &delete :x :y)
  (mplist! plist &get :x)"
  (declare (indent defun))
  (let ((fn-sym (intern (format "doom-mplist-%s" (substring (symbol-name action) 1)))))
    (when (fboundp fn-sym)
      (if (memq action '(&delete &put))
          `(setq ,var (,fn-sym ,var ,@args))
        `(,fn-sym ,var ,@args)))))

;;;###autoload
(defun doom-mplist-delete (plist &rest keys)
  "TODO"
  (if (apply 'doom-mplist-member plist keys)
      (let (forms)
        (while plist
          (if (not (and (keywordp (car plist)) (memq (car plist) keys)))
              (setq forms (append forms (list (pop plist))))
            (pop plist)
            (while (and plist (not (keywordp (car plist))))
              (pop plist))))
        forms)
    plist))

;;;###autoload
(defun doom-mplist-put (plist key value)
  "TODO"
  (when (doom-mplist-member plist key)
    (doom-mplist-delete plist key))
  (setq plist (append plist (list key value))))

;;;###autoload
(defun doom-mplist-get (plist &rest keys)
  "TODO"
  (if (cdr keys)
      (let ((keys (mapcar (lambda (key) (memq key plist)) keys))
            values)
        (dolist (forms keys)
          (when forms
            (push (cdr forms) values)))
        values)
    (cdr (memq (car keys) plist))))

;;;###autoload
(defun doom-mplist-member (plist &rest keys)
  (cl-some (lambda (key) (memq key plist)) keys))


