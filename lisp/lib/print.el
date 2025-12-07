;;; lisp/lib/print.el -*- lexical-binding: t; -*-
;;; Commentary
;;
;; This is Doom's output library, for controlling what does and doesn't get
;; logged, and provides a simple DSL for formatting output. It's mainly to
;; serve the noninteractive use-case, as `message' is more than good enough in
;; interactive sessions, but `print!' and `doom-log' are safe to use as a
;; drop-in replacement.
;;
;;; Code:
(eval-when-compile (require 'doom)) ; be silent, o'byte-compiler
(require 'ansi-color)


;;
;;; Variables

(defvar doom-print-ansi-alist
  '(;; fx
    (bold       . 1)
    (dark       . 2)
    (italic     . 3)
    (underscore . 4)
    (blink      . 5)
    (rapid      . 6)
    (contrary   . 7)
    (concealed  . 8)
    (strike     . 9)
    ;; fg
    (black      . 30)
    (red        . 31)
    (green      . 32)
    (yellow     . 33)
    (blue       . 34)
    (magenta    . 35)
    (cyan       . 36)
    (white      . 37)
    ;; bg
    (on-black   . 40)
    (on-red     . 41)
    (on-green   . 42)
    (on-yellow  . 43)
    (on-blue    . 44)
    (on-magenta . 45)
    (on-cyan    . 46)
    (on-white   . 47))
  "An alist of fg/bg/fx names mapped to ansi codes.

This serves as the cipher for converting (COLOR ...) function calls in `print!'
and `format!' into colored output, where COLOR is any car of this list (or
`doom-print-class-alist').")

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
    (path    . (lambda (&rest segments)
                 (abbreviate-file-name (apply #'doom-path segments))))
    (symbol  . symbol-name)
    (relpath . (lambda (str &optional dir)
                 (if (or (not str)
                         (not (stringp str))
                         (string-blank-p str))
                     str
                   (let* ((dir (or dir default-directory))
                          (str (expand-file-name str dir)))
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

(defvar doom-print-stream nil
  "The default value for `standard-output' for Doom's print API.

If non-nil, this is used instead of `standard-output' because changes to that
variable don't survive translation units.")

(defvar doom-print-level 'notice
  "The current, default logging level.")

(defvar doom-print-minimum-level 'notice
  "The minimum logging level for a message to be output.")

;; Record print levels in these symbols for easy, quasi-read-only access later.
(let ((levels '(debug    ; the system is thinking out loud
                info     ; less details about important progress
                notice   ; important details about important progress
                warning  ; a dismissable issue that may have reprecussions later
                error))) ; something has gone terribly wrong
  (dotimes (i (length levels))
    (put (nth i levels) 'print-level i)))


;;
;;; Library

;;;###autoload
(cl-defun doom-print
    (output &key
            (format nil)
            (level doom-print-level)
            (newline t)
            (stream (or doom-print-stream standard-output)))
  "Print OUTPUT to stdout.

Unlike `message', this:
- Respects the value of `standard-output' (if `doom-print-stream' is nil).
- Indents according to `doom-print-indent' (if FORMAT is non-nil).
- Prints to stdout instead of stderr in batch mode.
- Recognizes more terminal escape codes (only in batch mode).
- No-ops if OUTPUT is nil or an empty/blank string.

Returns OUTPUT."
  (cl-check-type output (or null string))
  (when (and (stringp output)
             (or (eq level t)
                 (if (listp level)
                     (memq doom-print-minimum-level level)
                   (>= (get level 'print-level)
                       (or (get doom-print-minimum-level 'print-level)
                           9999)))))
    (when format
      (setq output (doom-print--format "%s" output)))
    (princ output stream)
    (if newline (terpri stream))
    output))

;;;###autoload
(defmacro format! (message &rest args)
  "An alternative to `format' that understands `print!'s style syntax."
  `(doom-print--format ,@(doom-print--apply `(,message ,@args))))

;;;###autoload
(defmacro print-group! (&rest body)
  "Indents any `print!' or `format!' output within BODY."
  (declare (indent defun))
  (cl-destructuring-bind (&key if indent level verbose title
                               ;; TODO: Implement these
                               _benchmark)
      (cl-loop for (key val) on body by #'cddr
               while (keywordp key)
               collect (pop body)
               collect (pop body))
    (if verbose (setq level ''info))
    `(progn
       ,@(if title `((print! (start ,title))))
       (let ((doom-print-level (or ,level doom-print-level))
             (doom-print-indent
              (+ (if ,(or if t) (or ,indent doom-print-indent-increment) 0)
                 doom-print-indent)))
         ,@body))))

;;;###autoload
(defmacro print! (message &rest args)
  "Prints MESSAGE, formatted with ARGS, to stdout.

Returns non-nil if the message is a non-empty string.

Can be colored using (color ...) blocks:

  (print! \"Hello %s\" (bold (blue \"How are you?\")))
  (print! \"Hello %s\" (red \"World\"))
  (print! (green \"Great %s!\") \"success\")

Uses faces in interactive sessions and ANSI codes otherwise."
  `(doom-print (format! ,message ,@args)))

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

(defvar doom-print--output-depth 0)
;;;###autoload
(defmacro with-output-to! (streamspec &rest body)
  "Capture all output within BODY according to STREAMSPEC.

STREAMSPEC is a list of log specifications, indicating where to write output
based on the print level of the message. For example:

  `((>= notice ,(get-buffer-create \"*stdout*\"))
    (= error   ,(get-buffer-create \"*errors*\"))
    (t . ,(get-buffer-create \"*debug*\")))"
  (declare (indent 1))
  (let ((sym (make-symbol "streamspec")))
    `(letf! ((,sym ,streamspec)
             (standard-output (doom-print--redirect-standard-output ,sym t))
             (#'message (doom-print--redirect-message ,sym (if noninteractive 'debug 'notice)))
             (doom-print-stream standard-output))
       ,@body)))


;;
;;; Helpers

(defun doom-print--redirect-streams (streamspec level)
  (if (or (eq streamspec t)
          (bufferp streamspec)
          (functionp streamspec)
          (markerp streamspec))
      (list (cons t streamspec))
    (cl-loop for (car . spec) in streamspec
             if (eq car t)
             collect (cons t spec)
             else
             collect (cons (or (eq level t)
                               (doom-partial
                                car
                                (get level 'print-level)
                                (get (car spec) 'print-level)))
                           (cadr spec)))))

(defun doom-print--redirect-standard-output (streamspec level &optional old-stream)
  (let ((old (or old-stream standard-output))
        (streams (doom-print--redirect-streams streamspec level)))
    (lambda (ch)
      (let ((str (char-to-string ch)))
        (dolist (stream streams)
          (when (or (eq (car stream) t)
                    (funcall (car stream)))
            (doom-print str :newline nil :stream (cdr stream))))
        (doom-print str :newline nil :stream t :level level)))))

(defun doom-print--redirect-message (streamspec level)
  (let ((old (symbol-function #'message))
        (streams (doom-print--redirect-streams streamspec level))
        (doom-print--output-depth (1+ doom-print--output-depth)))
    (lambda (message &rest args)
      (when message
        (let ((output (apply #'doom-print--format message args)))
          (if (<= doom-print--output-depth 1)
              (doom-print output :level level :stream t)
            (let ((doom-print--output-depth (1- doom-print--output-depth)))
              (funcall old "%s" output)))
          (dolist (stream streams)
            (when (or (eq (car stream) t)
                      (funcall (car stream)))
              (doom-print output :stream (cdr stream)))))
        message))))

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
    (let* ((re "^\\( *\\)\r")
           (line-feed (if (stringp text) (string-match-p re text)))
           (width (cond ((null prefix) doom-print-indent-increment)
                        ((integerp prefix) prefix)
                        ((length (ansi-color-filter-apply (format "%s" prefix)))))))
      (insert
       (if text
           (replace-regexp-in-string re "\\1\033[K" (format "%s" text))
         ""))
      (indent-rigidly (point-min) (point-max) width)
      (save-excursion
        (when line-feed
          (goto-char (point-min))
          (insert "\r")))
      (save-excursion
        (when (stringp prefix)
          (goto-char (point-min))
          (delete-char (+ width (if line-feed 1 0)))
          (insert prefix)))
      (buffer-string))))

;;;###autoload
(defun doom-print--fill (message &optional column indent)
  "Ensure MSG is split into lines no longer than `fill-column'."
  (with-temp-buffer
    (let* ((indent (or indent 0))
           (fill-column (or column fill-column))
           (fill-prefix (make-string indent ?\s)))
      (save-excursion
        (insert (replace-regexp-in-string
                 "\n" "\n\n" (format "%s" (or message "")))))
      ;; HACK: Use `fill-region', but don't count ANSI codes as legitimate
      ;;   characters when calculating per-line `fill-column'.
      (letf! (defun current-fill-column ()
               (let ((target (funcall current-fill-column)))
                 (save-excursion
                   (goto-char (pos-bol))
                   (let ((n 0)
                         (c 0))
                     (while (and (not (eolp))
                                 (<= n target))
                       (save-match-data
                         (if (looking-at ansi-color-control-seq-regexp)
                             (let ((len (length (match-string 0))))
                               (cl-incf c len)
                               (forward-char len))
                           (cl-incf n 1)
                           (forward-char 1))))
                     (+ target c (length fill-prefix))))))
        (fill-region (point-min) (point-max) nil t))
      (replace-regexp-in-string
       "\n\n" "\n" (buffer-string)))))

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
  (let* ((code (cdr (assq style doom-print-ansi-alist)))
         (format (format "%s" (or format "")))
         (message (if args (apply #'format format args) format)))
    (unless code
      (error "Invalid print style: %s" style))
    (pcase doom-print-backend
      (`ansi
       (format "\e[0%dm%s\e[%dm" code message 0))
      (`text-properties
       (ansi-color-apply message))
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

(provide 'doom-lib '(print))
;;; print.el ends here
