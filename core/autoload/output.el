;;; core/autoload/output.el -*- lexical-binding: t; -*-

(defvar doom-output-ansi-alist
  '(;; fx
    (bold       1 :weight bold)
    (dark       2)
    (italic     3 :slant italic)
    (underscore 4 :underline t)
    (blink      5)
    (rapid      6)
    (contrary   7)
    (concealed  8)
    (strike     9 :strike-through t)
    ;; fg
    (black      30 term-color-black)
    (red        31 term-color-red)
    (green      32 term-color-green)
    (yellow     33 term-color-yellow)
    (blue       34 term-color-blue)
    (magenta    35 term-color-magenta)
    (cyan       36 term-color-cyan)
    (white      37 term-color-white)
    ;; bg
    (on-black   40 term-color-black)
    (on-red     41 term-color-red)
    (on-green   42 term-color-green)
    (on-yellow  43 term-color-yellow)
    (on-blue    44 term-color-blue)
    (on-magenta 45 term-color-magenta)
    (on-cyan    46 term-color-cyan)
    (on-white   47 term-color-white))
  "An alist of fg/bg/fx names mapped to ansi codes and term-color-* variables.

This serves as the cipher for converting (COLOR ...) function calls in `print!'
and `format!' into colored output, where COLOR is any car of this list.")

(defvar doom-output-class-alist
  `((color . doom--output-color)
    (class . doom--output-class)
    (indent . doom--output-indent)
    (autofill . doom--output-autofill)
    (success . (lambda (str &rest args)
                 (apply #'doom--output-color 'green (format "✓ %s" str) args)))
    (warn    . (lambda (str &rest args)
                 (apply #'doom--output-color 'yellow (format "! %s" str) args)))
    (error   . (lambda (str &rest args)
                 (apply #'doom--output-color 'red (format "x %s" str) args)))
    (info    . (lambda (str &rest args)
                 (concat "- " (if args (apply #'format str args) str))))
    (start    . (lambda (str &rest args)
                  (concat "> " (if args (apply #'format str args) str))))
    (debug   . (lambda (str &rest args)
                 (if doom-debug-p
                     (apply #'doom--output-color 'dark
                            (format "- %s" str)
                            args)
                   "")))
    (path    . abbreviate-file-name)
    (symbol . symbol-name)
    (relpath . (lambda (str &optional dir)
                 (if (or (not str)
                         (not (stringp str))
                         (string-empty-p str))
                     str
                   (let ((dir (or dir (file-truename default-directory)))
                         (str (file-truename str)))
                     (if (file-in-directory-p str dir)
                         (file-relative-name str dir)
                       (abbreviate-file-name str))))))
    (filename . file-name-nondirectory)
    (dirname . (lambda (path)
                 (unless (file-directory-p path)
                   (setq path (file-name-directory path)))
                 (directory-file-name path))))
  "An alist of text classes that map to transformation functions.

Any of these classes can be called like functions from within `format!' and
`print!' calls, which will transform their input.")

(defvar doom-output-indent 0
  "Level to rigidly indent text returned by `format!' and `print!'.")

(defvar doom-output-indent-increment 2
  "Steps in which to increment `doom-output-indent' for consecutive levels.")

(defvar doom-output-backend
  (if doom-interactive-p 'text-properties 'ansi)
  "Determines whether to print colors with ANSI codes or with text properties.

Accepts 'ansi and 'text-properties. nil means don't render colors.")


;;
;;; Library

;;;###autoload
(defun doom--format (output)
  (if (string-empty-p (string-trim output))
      ""
    (concat (make-string doom-output-indent 32)
            (replace-regexp-in-string
             "\n" (concat "\n" (make-string doom-output-indent 32))
             output t t))))

;;;###autoload
(defun doom--print (output)
  (unless (string-empty-p output)
    (if noninteractive
        (send-string-to-terminal output)
      (princ output))
    (terpri)
    output))

;;;###autoload
(defun doom--output-indent (width text &optional prefix)
  "Indent TEXT by WIDTH spaces. If ARGS, format TEXT with them."
  (with-temp-buffer
    (setq text (format "%s" text))
    (insert text)
    (indent-rigidly (point-min) (point-max) width)
    (when (stringp prefix)
      (when (> width 2)
        (goto-char (point-min))
        (beginning-of-line-text)
        (delete-char (- (length prefix)))
        (insert prefix)))
    (buffer-string)))

;;;###autoload
(defun doom--output-autofill (&rest msgs)
  "Ensure MSG is split into lines no longer than `fill-column'."
  (with-temp-buffer
    (let ((fill-column 76))
      (dolist (line msgs)
        (when line
          (insert (format "%s" line))))
      (fill-region (point-min) (point-max))
      (buffer-string))))

;;;###autoload
(defun doom--output-color (style format &rest args)
  "Apply STYLE to formatted MESSAGE with ARGS.

STYLE is a symbol that correlates to `doom-output-ansi-alist'.

In a noninteractive session, this wraps the result in ansi color codes.
Otherwise, it maps colors to a term-color-* face."
  (let* ((code (cadr (assq style doom-output-ansi-alist)))
         (format (format "%s" format))
         (message (if args (apply #'format format args) format)))
    (unless code
      (error "%S is an invalid color" style))
    (pcase doom-output-backend
      (`ansi
       (format "\e[%dm%s\e[%dm" code message 0))
      (`text-properties
       (require 'term)  ; piggyback on term's color faces
       (propertize
        message
        'face
        (append (get-text-property 0 'face format)
                (cond ((>= code 40)
                       `(:background ,(caddr (assq style doom-output-ansi-alist))))
                      ((>= code 30)
                       `(:foreground ,(face-foreground (caddr (assq style doom-output-ansi-alist)))))
                      ((cddr (assq style doom-output-ansi-alist)))))))
      (_ message))))

;;;###autoload
(defun doom--output-class (class format &rest args)
  "Apply CLASS to formatted format with ARGS.

CLASS is derived from `doom-output-class-alist', and can contain any arbitrary,
transformative logic."
  (let (fn)
    (cond ((setq fn (cdr (assq class doom-output-class-alist)))
           (if (functionp fn)
               (apply fn format args)
             (error "%s does not have a function" class)))
          (args (apply #'format format args))
          (format))))

;;;###autoload
(defun doom--output-apply (forms &optional sub)
  "Replace color-name functions with calls to `doom--output-color'."
  (cond ((null forms) nil)
        ((listp forms)
         (append (cond ((not (symbolp (car forms)))
                        (list (doom--output-apply (car forms))))
                       (sub
                        (list (car forms)))
                       ((assq (car forms) doom-output-ansi-alist)
                        `(doom--output-color ',(car forms)))
                       ((assq (car forms) doom-output-class-alist)
                        `(doom--output-class ',(car forms)))
                       ((list (car forms))))
                 (doom--output-apply (cdr forms) t)
                 nil))
        (forms)))

;;;###autoload
(defmacro format! (message &rest args)
  "An alternative to `format' that understands (color ...) and converts them
into faces or ANSI codes depending on the type of sesssion we're in."
  `(doom--format (format ,@(doom--output-apply `(,message ,@args)))))

;;;###autoload
(defmacro print-group! (&rest body)
  "Indents any `print!' or `format!' output within BODY."
  `(let ((doom-output-indent (+ doom-output-indent-increment doom-output-indent)))
     ,@body))

;;;###autoload
(defmacro print! (message &rest args)
  "Prints MESSAGE, formatted with ARGS, to stdout.

Returns non-nil if the message is a non-empty string.

Can be colored using (color ...) blocks:

  (print! \"Hello %s\" (bold (blue \"How are you?\")))
  (print! \"Hello %s\" (red \"World\"))
  (print! (green \"Great %s!\") \"success\")

Uses faces in interactive sessions and ANSI codes otherwise."
  `(doom--print (format! ,message ,@args)))

;;;###autoload
(defmacro insert! (message &rest args)
  "Like `insert'; the last argument must be format arguments for MESSAGE.

\(fn MESSAGE... ARGS)"
  `(insert (format! (concat ,message ,@(butlast args))
                    ,@(car (last args)))))

;;;###autoload
(defmacro error! (message &rest args)
  "Like `error', but with the power of `format!'."
  `(error (format! ,message ,@args)))

;;;###autoload
(defmacro user-error! (message &rest args)
  "Like `user-error', but with the power of `format!'."
  `(user-error (format! ,message ,@args)))

;;;###autoload
(defmacro with-output-to! (dest &rest body)
  "Send all output produced in BODY to DEST.
DEST can be one or more of `standard-output', a buffer, a file"
  (declare (indent 1))
  `(let* ((log-buffer (generate-new-buffer " *doom log*"))
          (standard-output
           (lambda (out)
             (with-current-buffer log-buffer
               (insert-char out))
             (send-string-to-terminal (char-to-string out)))))
     (letf! (defun message (msg &rest args)
              (when msg
                (print-group!
                 (with-current-buffer log-buffer
                   (insert (doom--format (apply #'format msg args)) "\n"))
                 (when (or doom-debug-p (not inhibit-message))
                   (doom--print (doom--format (apply #'format msg args))))))
              message)
       (unwind-protect
           ,(macroexp-progn body)
         (with-current-buffer log-buffer
           (require 'ansi-color)
           (ansi-color-filter-region (point-min) (point-max)))
         (let ((dest ,dest))
           (cond ((bufferp dest)
                  (with-current-buffer dest
                    (insert-buffer-substring log-buffer)))
                 ((stringp dest)
                  (make-directory (file-name-directory dest) 'parents)
                  (with-temp-file dest
                    (insert-buffer-substring log-buffer))))
           (kill-buffer log-buffer))))))
