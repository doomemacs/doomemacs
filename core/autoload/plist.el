;;; ../work/conf/doom-emacs/core/autoload/plist.el -*- lexical-binding: t; -*-

;;
;;; Macros

;;;###autoload
(cl-defmacro doplist! ((arglist plist &optional retval) &rest body)
  "Loop over a PLIST's (property value) pairs then return RETVAL.

Evaluate BODY with either ARGLIST bound to (cons PROP VAL) or, if ARGLIST is a
list, the pair is destructured into (CAR . CDR)."
  (declare (indent defun))
  (let ((plist-var (make-symbol "plist")))
    `(let ((,plist-var ,plist))
       (while ,plist-var
         (let ,(if (listp arglist)
                   `((,(pop arglist) (pop ,plist-var))
                     (,(pop arglist) (pop ,plist-var)))
                 `((,arglist (cons (pop ,plist-var)
                                   (pop ,plist-var)))))
           ,@body))
       ,retval)))

;;;###autoload
(defmacro plist-put! (plist prop value)
  "Set PROP to VALUE in PLIST in-place."
  `(setq ,plist (plist-put ,plist ,prop ,value)))

;;;###autoload
(defmacro plist-delete! (plist prop)
  "Delete PROP from PLIST in-place."
  `(setq ,plist (doom-plist-delete ,plist ,prop)))

;;;###autoload
(defmacro with-plist! (plist props &rest body)
  "With props bound from PLIST to PROPS, evaluate BODY.

PROPS is a list of symbols. Each one is converted to a keyword and then its
value is looked up in the PLIST and bound to the symbol for the duration of
BODY."
  (declare (indent 2))
  (let ((plist-sym (make-symbol "plist")))
    `(let* ((,plist-sym ,plist)
            ,@(cl-loop for prop in props
                       collect
                       `(,prop
                         (plist-get
                          ,plist-sym
                          ,(doom-keyword-intern (symbol-name prop))))))
       ,@body)))


;;
;;; Library

;;;###autoload
(defun doom-plist-get (plist prop &optional nil-value)
  "Return PROP in PLIST, if it exists. Otherwise NIL-VALUE."
  (if-let* ((val (plist-member plist prop)))
      (cadr val)
    nil-value))

;;;###autoload
(defun doom-plist-merge (from-plist to-plist)
  "Destructively merge FROM-PLIST onto TO-PLIST"
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
(defun doom-plist-delete (plist prop)
  "Delete PROP from a copy of PLIST."
  (let (p)
    (while plist
      (if (not (eq prop (car plist)))
          (plist-put! p (car plist) (nth 1 plist)))
      (setq plist (cddr plist)))
    p))
