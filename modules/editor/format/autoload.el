;;; editor/format/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +format--resolve-system (choices)
  "Get first choice matching `format-all-system-type' from CHOICES."
  (cl-loop for choice in choices
           if (atom choice) return choice
           else if (eql format-all-system-type (car choice))
           return (cadr choice)))

;;;###autodef
(cl-defun set-formatter! (modes formatter &key
                                name
                                install
                                filter
                                ok-statuses
                                error-regexp)
  "Define a FORMATTER for MODES.

MODES can be a major mode symbol, a vector of major modes, or a vector of
two-element vectors made up of [MAJOR-MODE FORM]. FORM is evaluated when the
buffer is formatted and its return value is a predicate for this formatter. Its
return value is stored in If it is non-nil, this formatter is used. Its return
value is stored in the `mode-result' variable for FORMATTER (if it's not a
string).

FORMATTER can be a function, string or nested vector.

  If a function, it should be a formatter function that
    `format-all-buffer-thunk' will accept.
  If a string, it is assumed to be a shell command that the text will be piped
    to (stdin).
  If a vector, it should represent a shell command as a list of arguments. Each
    element is either a string or vector [STRING ARG] where STRING is a format
    string and ARG is both a predicate and argument for STRING. If ARG is nil,
    STRING will be omitted from the vector.

NAME is the identifier for this formatter. If FORMATTER is a lambda, NAME will
default to \"default\".

INSTALL should be a string representing the shell command necessary to install
this formatter's dependencies. INSTALL can also be a list of lists made up of
two items: (OS COMMAND).

Basic examples:

  (set-formatter! '(asm-mode nasm-mode) \"asmfmt\")
  (set-formatter! 'python-mode \"black -q -\" :install \"pip install black\")

Advanced examples:

  (set-formatter!
    '((c-mode \".c\")
      (c++-mode \".cpp\")
      (java-mode \".java\")
      (objc-mode \".m\")
      (protobuf-mode \".proto\"))
    '(\"clang-format\"
      (\"-assume-filename=%S\" (or buffer-file-name mode-result \"\")))
    :install '(macos \"brew install clang-format\"))

  (set-formatter!
    '(html-mode
      (web-mode (and (equal \"none\" web-mode-engine)
                     (car (member web-mode-content-type '(\"xml\" \"html\"))))))
    '(\"tidy\" \"-q\" \"-indent\"
      (\"-xml\" (memq major-mode '(nxml-mode xml-mode))))
    :ok-statuses '(0 1)
    :install '(macos \"brew install tidy-html5\"))

  (set-formatter! 'elm-mode
    \"elm-format --yes --stdin\"
    :install '(macos \"brew install elm\")
    :filter
    (lambda (output errput first-diff)
      (list output
            (format-all-remove-ansi-color errput)
            first-diff)))"
  (declare (indent defun))
  (let* ((command-list (cond ((stringp formatter)   ; shell command
                              (split-string formatter " " t))
                             ((listp formatter)     ; shell command in lists
                              formatter)))
         (name (cond ((or name (car command-list)))
                     ((symbolp formatter) (symbol-name formatter))
                     ("default"))))
    (after! format-all
      (puthash
       name
       (lambda (executable mode-result)
         (ignore mode-result executable)
         (apply (or filter #'identity)
                (cond ((commandp formatter)
                       (let ((mode major-mode)
                             (file buffer-file-name))
                         (format-all-buffer-thunk
                          (lambda (input)
                            (setq buffer-file-name file)
                            (funcall mode)
                            (insert input)
                            (condition-case e
                                (progn
                                  (call-interactively formatter)
                                  (list nil ""))
                              (error (list t (error-message-string e))))))))
                      ((functionp formatter)
                       (format-all-buffer-thunk formatter))
                      ((let ((args (cl-loop for arg in command-list
                                            if (stringp arg) collect arg
                                            else if
                                            (and (listp arg)
                                                 (eval (cadr arg) t))
                                            collect (format (car arg) it))))
                         (if (or ok-statuses error-regexp)
                             (apply #'format-all-buffer-hard ok-statuses error-regexp args)
                           (apply #'format-all-buffer-easy args))))))) format-all-format-table)
      (puthash name (cond ((null install) install)
                          ((listp install) (cdr (assq (+format--resolve-system) install)))
                          (install))
               format-all-install-table)
      (puthash name (car command-list) format-all-executable-table)
      (dolist (mode (doom-enlist modes))
        (cl-destructuring-bind (m &optional probe)
            (doom-enlist mode)
          (format-all-pushhash
           m (cons name (if probe `(lambda () ,probe)))
           format-all-mode-table))))
    name))

;;;###autoload
(defun +format-region (beg end)
  "TODO"
  (cl-check-type beg integer)
  (cl-check-type end integer)
  (save-restriction
    (let* ((beg (save-excursion (goto-char beg) (line-beginning-position)))
           (end (save-excursion (goto-char end) (line-end-position)))
           (file buffer-file-name)
           (input (buffer-substring-no-properties beg end)))
      (with-temp-buffer
        (setq buffer-file-name file)
        (insert input)
        (format-all-buffer)))))


;;
;; Commands
;;

;;;###autoload
(defalias '+format/buffer 'format-all-buffer)

;;;###autoload
(defun +format/region (beg end)
  "TODO"
  (interactive "r")
  (+format-region beg end))

;;;###autoload
(defun +format/region-or-buffer (beg end)
  "TODO"
  (interactive "r")
  (if (use-region-p)
      (+format-region beg end)
    (format-all-buffer)))
