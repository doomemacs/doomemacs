;;; plist.el
(provide 'doom-lib-plist)

;;;###autoload
(defmacro @plist (var action &rest args)
  "A helper macro that makes dealing with doom-plist-* functions a little more concise.

Examples:
  (@plist plist &delete :x :y)
  (@plist plist &get :x)"
  (declare (indent defun))
  (let ((fn-sym (intern (format "doom-plist-%s" (substring (symbol-name action) 1)))))
    (when (fboundp fn-sym)
      (if (memq action '(&delete &put))
          `(setq ,var (,fn-sym ,var ,@args))
        `(,fn-sym ,var ,@args)))))

;;;###autoload
(defun doom-plist-delete (plist &rest keys)
  (if (apply 'doom-plist-member plist keys)
      (let (forms)
        (while plist
          (if (and (keywordp (car plist)) (memq (car plist) keys))
              (progn
                (pop plist)
                (while (and plist (not (keywordp (car plist))))
                  (pop plist)))
            (setq forms (append forms (list (pop plist))))))
        forms)
    plist))

;;;###autoload
(defun doom-plist-put (plist key value &rest key-values))

;;;###autoload
(defun doom-plist-get (plist &rest keys))

;;;###autoload
(defun doom-plist-member (plist &rest keys)
  (--any-p (memq it plist) keys))


;; (let ((a '(abc :commands 1 :config 3 4 5)))
;;   (@plist a &delete :commands :config))
