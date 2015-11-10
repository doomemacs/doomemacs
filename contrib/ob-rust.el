;;; ob-rust.el --- org-babel functions for rust

;; Author: Philip Munksgaard
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;;; Commentary:

;; Org-Babel support for evaluating rust code.
;;
;; This is simply an adaptation of ob-C.el that instead targets rust code.
;; Thanks to the original creators Eric Schulte and Thierry Banel
;;
;; Very limited implementation:
;; - currently only support :results output
;; - not much in the way of error feedback

;;; Code:
(eval-when-compile
  (require 'cl))
(require 'ob)
(require 'cc-mode)

(declare-function org-entry-get "org"
		  (pom property &optional inherit literal-nil))


(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("rust"))

(defvar org-babel-default-header-args:rust '())

(defvar org-babel-rust-compiler "rustc"
  "Command used to compile a rust source code file into an
executable.")

(defun org-babel-expand-body:rust (body params)
  (org-babel-rust-expand body params))

(defun org-babel-execute:rust (body params)
  "Execute a block of rust code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (print body)
  (princ params)
  (let* ((tmp-src-file (org-babel-temp-file
			"rust-src-"
                        ".rs"))
         (tmp-bin-file (org-babel-temp-file "rust-bin-" org-babel-exeext))
         (cmdline (cdr (assoc :cmdline params)))
         (flags (cdr (assoc :flags params)))
         (full-body (org-babel-rust-expand body params))
         (compile
	  (progn
	    (with-temp-file tmp-src-file (insert full-body))
	    (org-babel-eval
	     (format "%s -o %s %s %s"
		     org-babel-rust-compiler
		     (org-babel-process-file-name tmp-bin-file)
		     (mapconcat 'identity
				(if (listp flags) flags (list flags)) " ")
		     (org-babel-process-file-name tmp-src-file)) ""))))
    (let ((results
           (org-babel-trim
            (org-babel-eval
             (concat tmp-bin-file (if cmdline (concat " " cmdline) "")) ""))))
      (org-babel-reassemble-table
       (org-babel-result-cond (cdr (assoc :result-params params))
	 (org-babel-read results)
         (let ((tmp-file (org-babel-temp-file "rust-")))
           (with-temp-file tmp-file (insert results))
           (org-babel-import-elisp-from-file tmp-file)))
       (org-babel-pick-name
        (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
       (org-babel-pick-name
        (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))
    ))

(defun org-babel-rust-expand (body params)
  "Expand a block of rust code with org-babel."
  (print params)
  (print body)
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var)))
        (main-p (not (string= (cdr (assoc :main params)) "no")))
        (uses (or (cdr (assoc :uses params))
                      (org-babel-read (org-entry-get nil "uses" t))))
        (externs (org-babel-read
                  (or (cdr (assoc :externs params))
                      (org-babel-read (org-entry-get nil "externs" t)))))
        (attributes (org-babel-read
                     (or (cdr (assoc :attributes params))
                         (org-babel-read (org-entry-get nil "attributes" t))))))
    (mapconcat 'identity
	       (list
		;; attributes
		(mapconcat
		 (lambda (attr) (format "#[%s]" attr))
		 (if (listp attributes) attributes (list attributes)) "\n")
		;; externs
		(mapconcat
		 (lambda (ext) (format "extern crate %s;" ext))
		 (if (listp externs) externs (list externs)) "\n")
                ;; uses
		(mapconcat
		 (lambda (use) (format "use %s;" use))
		 (if (listp uses) uses (list uses)) "\n")
		;; variables
		(mapconcat 'org-babel-rust-var-to-rust vars "\n")
		;; body
		(if main-p
		    (org-babel-rust-ensure-main-wrap body)
		  body) "\n") "\n")))

(defun org-babel-rust-ensure-main-wrap (body)
  "Wrap BODY in a \"main\" function call if none exists."
  (if (string-match "[ \t]*fn+[ \t\n\r]*main[ \t]*(.*)" body)
      body
    (format "fn main() {\n%s\n}\n" body)))

(defun org-babel-prep-session:rust (session params)
  "This function does nothing as rust is a compiled language with no
support for sessions"
  (error "rust is a compiled languages -- no support for sessions"))

(defun org-babel-load-session:rust (session body params)
  "This function does nothing as rust is a compiled language with no
support for sessions"
  (error "rust is a compiled languages -- no support for sessions"))

;; helper functions

(defun org-babel-rust-format-val (type val)
  "Handle the FORMAT part of TYPE with the data from VAL."
  (let ((format-data (cadr type)))
    (if (stringp format-data)
	(cons "" (format format-data val))
      (funcall format-data val))))

(defun org-babel-rust-val-to-rust-type (val)
  "Determine the type of VAL.
Return a list (TYPE-NAME FORMAT).  TYPE-NAME should be the name of the type.
FORMAT can be either a format string or a function which is called with VAL."
  (cond
   ((integerp val) '("int" "%d"))
   ((floatp val) '("f64" "%f"))
   ((or (listp val) (vectorp val))
    (lexical-let ((type (org-babel-rust-val-to-rust-list-type val)))
      (list (concat "&'static [" (car type) "]")
	    (lambda (val)
	      (cons
	       (format "[%d]%s"
		       (length val)
		       (car (org-babel-rust-format-val type (elt val 0))))
	       (concat "&["
		       (mapconcat (lambda (v)
				    (cdr (org-babel-rust-format-val type v)))
				  val
				  ", ")
		       "]"))))))
   (t ;; treat unknown types as string
    '("&'static str" (lambda (val)
	       (let ((s (format "%s" val))) ;; convert to string for unknown types
		 (cons (format "[%d]" (1+ (length s)))
		       (concat "\"" s "\""))))))))

(defun org-babel-rust-val-to-rust-list-type (val)
  "Determine the rust array type of a VAL."
  (let (type)
    (mapc
     #'(lambda (i)
	 (let* ((tmp-type (org-babel-rust-val-to-rust-type i))
		(type-name (car type))
		(tmp-type-name (car tmp-type)))
	   (when (and type (not (string= type-name tmp-type-name)))
	     (if (and (member type-name '("int" "f64"))
		      (member tmp-type-name '("int" "f64")))
		 (setq tmp-type '("f64" "" "%f"))
	       (error "Only homogeneous lists are supported by rust.  You can not mix %s and %s"
		      type-name
		      tmp-type-name)))
	   (setq type tmp-type)))
     val)
    type))

(defun org-babel-rust-var-to-rust (pair)
  "Convert an elisp val into a string of rust code specifying a var
of the same value."
  ;; TODO list support
  (let ((var (car pair))
        (val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
        (setq val (string-to-char val))))
    (let* ((type-data (org-babel-rust-val-to-rust-type val))
	   (type (car type-data))
	   (formated (org-babel-rust-format-val type-data val))
	   ;; (suffix (car formated))
	   (data (cdr formated)))
      (format "static %s: %s = %s;"
              var
	      type
              ;suffix
	      data))))

(provide 'ob-rust)

;;; ob-rust.el ends here
