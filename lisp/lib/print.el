;;; lisp/lib/print.el -*- lexical-binding: t; -*-
;;; Commentary
;;;
;;; This is Doom's output library, for controlling what does and doesn't get
;;; logged, and provides a simple DSL for formatting output. It's mainly to
;;; serve the noninteractive use-case, as `message' is more than good enough in
;;; interactive sessions, but `print!' and `doom-log' are safe to use as a
;;; drop-in replacement.
;;;
;;; Code:

(require 'ansi-color)

(defvar doom-print-ansi-alist
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

(defvar doom-print-class-alist
  `((buffer  . doom-print--buffer)
    (color   . doom-print--style)
    (class   . doom-print--class)
    (indent  . doom-print--indent)
    (fill    . doom-print--fill)
    (join    . doom-print--join)
    (org     . doom-print--org)
    (markup  . doom-print--cli-markup)
    (trim    . string-trim)
    (rtrim   . string-trim-right)
    (ltrim   . string-trim-left)
    (p       . doom-print--paragraph)
    (buffer  . (lambda (buffer)
                 (with-current-buffer buffer
                   (buffer-string))))
    (truncate . doom-print--truncate)
    (success . (lambda (str &rest args)
                 (apply #'doom-print--style 'green
                        (doom-print--indent str "✓ ")
                        args)))
    (warn    . (lambda (str &rest args)
                 (apply #'doom-print--style 'yellow
                        (doom-print--indent str "! ")
                        args)))
    (error   . (lambda (str &rest args)
                 (apply #'doom-print--style 'red
                        (doom-print--indent str "x ")
                        args)))
    (item    . (lambda (str &rest args)
                 (doom-print--indent
                  (if args (apply #'format str args) str)
                  "- ")))
    (start   . (lambda (str &rest args)
                 (doom-print--indent
                  (if args (apply #'format str args) str)
                  "> ")))
    (path    . abbreviate-file-name)
    (symbol  . symbol-name)
    (relpath . (lambda (str &optional dir)
                 (if (or (not str)
                         (not (stringp str))
                         (string-blank-p str))
                     str
                   (let ((dir (or dir (file-truename default-directory)))
                         (str (file-truename str)))
                     (if (file-in-directory-p str dir)
                         (file-relative-name str dir)
                       (abbreviate-file-name str))))))
    (filename . file-name-nondirectory)
    (dirname  . (lambda (path)
                  (unless (file-directory-p path)
                    (setq path (file-name-directory path)))
                  (directory-file-name path))))
  "An alist of text classes that map to transformation functions.

Any of these classes can be called like functions from within `format!' and
`print!' calls, which will transform their input.")

(defvar doom-print-indent 0
  "Level to rigidly indent text returned by `format!' and `print!'.")

(defvar doom-print-indent-increment 2
  "Steps in which to increment `doom-print-indent' for consecutive levels.")

(defvar doom-print-backend (if noninteractive 'ansi 'text-properties)
  "Whether to print colors/styles with ANSI codes or with text properties.

Accepts `ansi' and `text-properties'. `nil' means don't render styles at all.")

(defvar doom-print-level (if init-file-debug 'debug 'info)
  "The default level of messages to print.")

(defvar doom-print-logging-level 'debug
  "The default logging level used by `doom-log'/`doom-print'.")

(defvar doom-print-message-level (if noninteractive 'debug 'info)
  "The default logging level used by `message'.")

(defvar doom-print--levels
  '(debug    ; the system is thinking out loud
    info     ; a FYI; to keep you posted
    warning  ; a dismissable issue that may have reprecussions later
    error))  ; functionality has been disabled by misbehavior

(dotimes (i (length doom-print--levels))
  (put (nth i doom-print--levels) 'level i))


;;
;;; Library

;;;###autoload
(cl-defun doom-print
    (output &key
            (format t)
            (newline t)
            (stream standard-output)
            (level doom-print-level))
  "Print OUTPUT to stdout.

Unlike `message', this:
- Respects `standard-output'.
- Respects `doom-print-indent' (if FORMAT)
- Prints to stdout instead of stderr in batch mode.
- Respects more ANSI codes (only in batch mode).
- No-ops if OUTPUT is nil or an empty/blank string.

Returns OUTPUT."
  (cl-check-type output (or null string))
  (when (and (stringp output)
             (not (string-blank-p output))
             (or (eq level t)
                 (>= (get level 'level)
                     (get doom-print-level 'level))))
    (let ((output (if format
                      (doom-print--format "%s" output)
                    output)))
      (princ output stream)
      (if newline (terpri stream))
      output)))

;;;###autoload
(progn
  ;; Autoload whole definition, so its buried uses don't pull in this whole file
  ;; with them at expansion time.
  (defmacro doom-log (output &rest args)
    "Log a message in *Messages*.

Does not emit the message in the echo area. This is a macro instead of a
function to prevent the potentially expensive execution of its arguments when
debug mode is off."
    `(when (or init-file-debug noninteractive)
       (let ((inhibit-message t))
         (message
          "%s" (propertize
                (doom-print--format
                 (format
                  "* [%s] %s"
                  ,(let ((time `(format "%.06f" (float-time (time-subtract (current-time) before-init-time)))))
                     (cond (noninteractive time)
                           ((bound-and-true-p doom--current-module)
                            (format "[:%s %s] "
                                    (doom-keyword-name (car doom--current-module))
                                    (cdr doom--current-module)))
                           ((when-let (file (ignore-errors (file!)))
                              (format "[%s] "
                                      (file-relative-name
                                       file (expand-file-name "../" (file-name-directory file))))))
                           (time)))
                  ,output)
                 ,@args)
                'face 'font-lock-doc-face))))))

;;;###autoload
(defmacro format! (message &rest args)
  "An alternative to `format' that understands (color ...) and converts them
into faces or ANSI codes depending on the type of sesssion we're in."
  `(doom-print--format ,@(doom-print--apply `(,message ,@args))))

;;;###autoload
(defmacro print-group! (&rest body)
  "Indents any `print!' or `format!' output within BODY."
  `(print-group-if! t ,@body))

;;;###autoload
(defmacro print-group-if! (condition &rest body)
  "Indents any `print!' or `format!' output within BODY."
  (declare (indent 1))
  `(let ((doom-print-indent
          (+ (if ,condition doom-print-indent-increment 0)
             doom-print-indent)))
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
  `(doom-print (format! ,message ,@args) :format nil))

;;;###autoload
(defmacro insert! (&rest args)
  "Like `insert', but with the power of `format!'.

Each argument in ARGS can be a list, as if they were arguments to `format!':
\(MESSAGE [ARGS...]).

\(fn &rest (MESSAGE . ARGS)...)"
  `(insert ,@(cl-loop for arg in args
                      if (listp arg)
                      collect `(format! ,@arg)
                      else collect arg)))


;;
;;; Helpers

;;;###autoload
(defun doom-print--format (message &rest args)
  (if (or (null message) (string-blank-p message))
      ""
    (concat (make-string doom-print-indent 32)
            (replace-regexp-in-string
             "\n" (concat "\n" (make-string doom-print-indent 32))
             (if args (apply #'format message args) message)
             t t))))

;;;###autoload
(defun doom-print--indent (text &optional prefix)
  "Indent TEXT by WIDTH spaces. If ARGS, format TEXT with them."
  (with-temp-buffer
    (let ((width
           (cond ((null prefix)
                  doom-print-indent-increment)
                 ((integerp prefix)
                  prefix)
                 ((length (ansi-color-filter-apply (format "%s" prefix)))))))
      (insert (format "%s" (or text "")))
      (indent-rigidly (point-min) (point-max) width)
      (when (stringp prefix)
        (goto-char (point-min))
        (delete-char width)
        (insert prefix))
      (buffer-string))))

;;;###autoload
(defun doom-print--fill (message &optional column indent)
  "Ensure MSG is split into lines no longer than `fill-column'."
  (with-temp-buffer
    (let* ((fill-column (or column fill-column))
           (col 0)
           (indent (or indent 0))
           (fill-prefix (make-string indent ?\s)))
      (save-excursion
        (insert (format "%s" (or message ""))))
      ;; HACK This monkey patches `fill-region' to not count ANSI codes as
      ;;   legitimate characters, when calculating per-line `fill-column'.
      (letf! (defun current-fill-column ()
               (let ((target (funcall current-fill-column)))
                 (save-excursion
                   (goto-char (line-beginning-position))
                   (let ((n 0)
                         (c 0))
                     (while (and (not (eolp)) (<= n target))
                       (save-match-data
                         (if (looking-at ansi-color-control-seq-regexp)
                             (let ((len (length (match-string 0))))
                               (cl-incf c len)
                               (forward-char len))
                           (cl-incf n 1)
                           (forward-char 1))))
                     (+ target c (length fill-prefix))))))
        (fill-region (point-min) (point-max) nil t))
      (buffer-string))))

;;;###autoload
(defun doom-print--paragraph (&rest lines)
  "TODO"
  (doom-print--fill (apply #'concat lines)))

;;;###autoload
(defun doom-print--join (sequence &optional separator)
  "Ensure SEQUENCE is joined with SEPARATOR.

`nil' and empty strings in SEQUENCE are omitted."
  (mapconcat (doom-partial #'format "%s")
             (seq-remove (fn! (or (null %)
                                  (and (stringp %)
                                       (string-empty-p %))))
                         sequence)
             (or separator " ")))

;;;###autoload
(defun doom-print--truncate (text &optional col ellipsis)
  "Replaces basic org markup with ansi/text-properties."
  (truncate-string-to-width (or text "") (or col (- fill-column doom-print-indent))
                            nil nil (or ellipsis "...")))

;;;###autoload
(defun doom-print--buffer (buffer &optional beg end)
  "Replaces basic org markup with ansi/text-properties."
  (if (and (bufferp buffer) (buffer-live-p buffer))
      (with-current-buffer buffer
        (if (or beg end)
            (buffer-substring (or beg (point-min))
                              (or end (point-max)))
          (buffer-string)))
    ""))

;;;###autoload
(defun doom-print--cli-markup (text)
  "Replace `...', `...`, and ```...``` quotes in TEXT with CLI formatting.

- `$ENVVAR' = bolded
- `--switch' = bolded
- `ARG' = underlined
- `symbol' = highlighted in blue
- `arbitrary code` = highlighted in blue
- ```
  Arbitrary multiline code gets highlighted in blue too.
  ```"
  (if (not text) ""
    (let ((case-fold-search nil))
      ;; TODO Syntax highlighting?
      (replace-regexp-in-string
       " *```\n\\(.+?\\)\n *```" (doom-print--style 'blue "%s" "\\1")
       (replace-regexp-in-string
        "`\\$ \\([^`\n]+?\\)`" (format "`%s`" (doom-print--style 'blue "%s" "\\1"))
        (replace-regexp-in-string
         "`\\([^ \n]+?\\)'"
         (let ((styles '(("^\\$" . envvar)
                         ("^--?" . option)
                         ("^[A-Z][A-Z0-9-_]*$" . arg)
                         ("." . symbol))))
           (lambda (match)
             (let ((text (match-string 1 match)))
               (pcase (assoc-default text styles #'string-match-p)
                 (`arg    (doom-print--style 'underscore "%s" text))
                 (`envvar (doom-print--style 'bold "%s" text))
                 (`option (doom-print--style 'bold "%s" text))
                 (_ (format "`%s'" (doom-print--style 'blue "%s" text)))))))
         text t)
        t)
       t))))

;;;###autoload
(defun doom-print--org (text)
  "Replaces basic Org markup with ansi/text-properties.

All emphasis markers need to be preceded by a backslash."
  (let* ((inhibit-modification-hooks t)
         (styles '((?* . bold)
                   (?_ . underscore)
                   (?/ . italic)
                   (?= . magenta)
                   (?+ . strike)
                   (?~ . blue)))
         (fences (regexp-quote (mapconcat #'char-to-string (mapcar #'car styles) ""))))
    (with-temp-buffer
      (save-excursion (insert text))
      (while (re-search-forward (format "\\([%s]\\)" fences) nil t)
        (unless (= (char-before (match-beginning 0)) ?\\)
          (let* ((beg (match-beginning 0))
                 (ibeg (point))
                 (fence (match-string 1))
                 (fence-re (regexp-quote fence)))
            (when (re-search-forward (format "[^\\]%s" fence-re) (line-end-position 2) t)
              (let ((end (point))
                    (iend (1- (point))))
                (let ((text (buffer-substring ibeg iend)))
                  (when-let (style (cdr (assq (string-to-char fence) styles)))
                    (goto-char beg)
                    (delete-region beg end)
                    (insert (doom-print--style style "%s" text)))))
              (goto-char beg)))))
      (buffer-string))))

;;;###autoload
(defun doom-print--style (style format &rest args)
  "Apply STYLE to formatted MESSAGE with ARGS.

STYLE is a symbol that correlates to `doom-print-ansi-alist'.

In a noninteractive session, this wraps the result in ansi color codes.
Otherwise, it maps colors to a term-color-* face."
  (let* ((code (cadr (assq style doom-print-ansi-alist)))
         (format (format "%s" (or format "")))
         (message (if args (apply #'format format args) format)))
    (unless code
      (error "Invalid print style: %s" style))
    (pcase doom-print-backend
      (`ansi
       (format "\e[0%dm%s\e[%dm" code message 0))
      (`text-properties
       (require 'term)  ; piggyback on term's color faces
       (propertize
        message
        'face
        (append (get-text-property 0 'face format)
                (cond ((>= code 40)
                       `(:background ,(caddr (assq style doom-print-ansi-alist))))
                      ((>= code 30)
                       `(:foreground ,(face-foreground (caddr (assq style doom-print-ansi-alist)))))
                      ((cddr (assq style doom-print-ansi-alist)))))))
      (_ message))))

;;;###autoload
(defun doom-print--class (class format &rest args)
  "Apply CLASS to formatted format with ARGS.

CLASS is derived from `doom-print-class-alist', and can contain any arbitrary,
transformative logic."
  (let (fn)
    (cond ((setq fn (cdr (assq class doom-print-class-alist)))
           (if (functionp fn)
               (apply fn format args)
             (error "%s does not have a function" class)))
          (args (apply #'format format args))
          (format))))

(defun doom-print--apply (forms &optional sub)
  "Replace color-name functions with calls to `doom-print--style'."
  (cond ((null forms) nil)
        ((listp forms)
         (append (cond ((not (symbolp (car forms)))
                        (list (doom-print--apply (car forms))))
                       (sub
                        (list (car forms)))
                       ((assq (car forms) doom-print-ansi-alist)
                        `(doom-print--style ',(car forms)))
                       ((assq (car forms) doom-print-class-alist)
                        `(doom-print--class ',(car forms)))
                       ((list (car forms))))
                 (doom-print--apply (cdr forms) t)
                 nil))
        (forms)))
