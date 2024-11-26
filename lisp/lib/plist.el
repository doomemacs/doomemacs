;;; lisp/lib/plist.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Macros

;;; DEPRECATED In favor of `cl-callf'
;;;###autoload
(defmacro plist-put! (plist &rest rest)
  "Set each PROP VALUE pair in REST to PLIST in-place."
  `(cl-loop for (prop value)
            on (list ,@rest) by #'cddr
            do ,(if (symbolp plist)
                    `(setq ,plist (plist-put ,plist prop value))
                  `(plist-put ,plist prop value))))


;;
;;; Library

;;;###autoload
(define-obsolete-function-alias 'doom-plist-get #'cl-getf "3.0.0")

;;;###autoload
(defun doom-plist-map (fn plist)
  "Map FN on each keyword/value pair in PLIST.

FN is a function that takes two arguments: a keyword and value, and its return
values are accumulated ala `mapcar'."
  (cl-loop for (key val) on plist by #'cddr
           while (keywordp key)
           do (plist-put plist key (funcall fn key val))))

;;;###autoload
(defun doom-plist-map* (fn vplist)
  "Apply FN to each variadic property in VPLIST.

FN is a variadic function, whose first argument is the keyword and the rest the
values that follow (until the next keyword). Its return value is accumulated ala
`mapcar'.

VPLIST is a variadic-property list (a plist whose key may be followed by one or
more values)."
  (let ((vplist (copy-sequence vplist))
        results)
    (while vplist
      (let ((prop (pop vplist))
            vals)
        (while (and vplist (not (keywordp (car vplist))))
          (push (pop vplist) vals))
        (push (funcall fn prop (nreverse vals)) results)))
    (nreverse results)))

;;;###autoload
(defun doom-plist-merge (from-plist to-plist)
  "Non-destructively merge FROM-PLIST onto TO-PLIST"
  (let ((from-plist (copy-sequence from-plist))
        (to-plist (copy-sequence to-plist)))
    (while from-plist
      (cl-callf plist-put to-plist (pop from-plist) (pop from-plist)))
    to-plist))

;;;###autoload
(defun doom-plist-delete-nil (plist)
  "Delete `nil' properties from a copy of PLIST."
  (let (p)
    (while plist
      (if (car plist)
          (cl-callf plist-put p (car plist) (nth 1 plist)))
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

(provide 'doom-lib '(plist))
;;; plist.el ends here
