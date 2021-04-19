;;; core/autoload/plist.el -*- lexical-binding: t; -*-

;;
;;; Macros

;;;###autoload
(cl-defmacro doplist! ((arglist plist &optional retval) &rest body)
  "Loop over a PLIST's (property value) pairs then return RETVAL.

Evaluate BODY with either ARGLIST bound to (cons PROP VAL) or, if ARGLIST is a
list, the pair is destructured into (CAR . CDR)."
  (declare (indent 1))
  (let ((plist-var (make-symbol "plist")))
    `(let ((,plist-var (copy-sequence ,plist)))
       (while ,plist-var
         (let ,(if (listp arglist)
                   `((,(pop arglist) (pop ,plist-var))
                     (,(pop arglist) (pop ,plist-var)))
                 `((,arglist (cons (pop ,plist-var)
                                   (pop ,plist-var)))))
           ,@body))
       ,retval)))

;;;###autoload
(defmacro plist-put! (plist &rest rest)
  "Set each PROP VALUE pair in REST to PLIST in-place."
  `(cl-loop for (prop value)
            on (list ,@rest) by #'cddr
            do ,(if (symbolp plist)
                    `(setq ,plist (plist-put ,plist prop value))
                  `(plist-put ,plist prop value))))

;;;###autoload
(defmacro plist-delete! (plist prop)
  "Delete PROP from PLIST in-place."
  `(setq ,plist (doom-plist-delete ,plist ,prop)))


;;
;;; Library

;;;###autoload
(defun doom-plist-get (plist prop &optional nil-value)
  "Return PROP in PLIST, if it exists. Otherwise NIL-VALUE."
  (if-let (val (plist-member plist prop))
      (cadr val)
    nil-value))

;;;###autoload
(defun doom-plist-merge (from-plist to-plist)
  "Non-destructively merge FROM-PLIST onto TO-PLIST"
  (let ((plist (copy-sequence from-plist)))
    (while plist
      (plist-put! to-plist (pop plist) (pop plist)))
    to-plist))

;;;###autoload
(defun doom-plist-delete-nil (plist)
  "Delete `nil' properties from a copy of PLIST."
  (let (p)
    (while plist
      (if (car plist)
          (plist-put! p (car plist) (nth 1 plist)))
      (setq plist (cddr plist)))
    p))

;;;###autoload
(defun doom-plist-delete (plist &rest props)
  "Delete PROPS from a copy of PLIST."
  (let (p)
    (while plist
      (if (not (memq (car plist) props))
          (plist-put! p (car plist) (nth 1 plist)))
      (setq plist (cddr plist)))
    p))

;;;###autoload
(defun doom-plist-keys (plist)
  "Return the keys in PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))

;;;###autoload
(defun doom-plist-values (plist)
  "Return the values in PLIST."
  (let (keys)
    (while plist
      (push (cadr plist) keys)
      (setq plist (cddr plist)))
    keys))
