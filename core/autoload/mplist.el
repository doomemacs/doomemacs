;;; plist.el
(provide 'doom-lib-mplist)

;; The built-in plist library won't work when the list is a malformed plist,
;; which is a plist that has multiple (or no) forms following its properties.
;; For example: (:x 5 :y 1 2 3 :z).
;;
;; This library was written for mplists, but use the standard plist-* functions
;; for regular plists -- they're faster.
;;
;; (let ((a '(abc :commands 1 :config 3 4 5)))
;;   (mplist! a &delete :commands :config))

;;;###autoload
(defmacro mplist! (var action &rest args)
  "A convenience macro for dealing with doom-mplist-* functions.

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
(defun doom-mplist-delete (mplist &rest keys)
  "Thoroughly removes a property and its values from an mplist."
  (if (apply 'doom-mplist-member mplist keys)
      (let (forms)
        (while mplist
          (if (not (and (keywordp (car mplist)) (memq (car mplist) keys)))
              (setq forms (append forms (list (pop mplist))))
            (pop mplist)
            (while (and mplist (not (keywordp (car mplist))))
              (pop mplist))))
        forms)
    mplist))

;;;###autoload
(defun doom-mplist-put (mplist key value)
  "Like `plist-put', but for mplists."
  (when (doom-mplist-member mplist key)
    (doom-mplist-delete mplist key))
  (setq mplist (append mplist (list key value))))

;;;###autoload
(defun doom-mplist-get (mplist &rest keys)
  "Like `plist-get', but for mplists. If KEYS is only one keyword, return the
mplist associated with it. If KEYS is multiple, return a list of associated
mplists."
  (if (cdr keys)
      (let ((keys (mapcar (lambda (key) (memq key mplist)) keys))
            values)
        (dolist (forms keys)
          (when forms
            (push (cdr forms) values)))
        values)
    (cdr (memq (car keys) mplist))))

;;;###autoload
(defun doom-mplist-member (mplist &rest keys)
  "Return t if any of KEYS (keywords) are in MPLIST."
  (cl-some (lambda (key) (memq key mplist)) keys))


