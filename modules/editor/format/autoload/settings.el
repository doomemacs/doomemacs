;;; editor/format/autoload/settings.el -*- lexical-binding: t; -*-

(defconst +format-system-type
  (cl-case system-type
    (windows-nt 'windows)
    (cygwin     'windows)
    (darwin     'macos)
    (gnu/linux  'linux)
    (berkeley-unix
     (save-match-data
       (let ((case-fold-search t))
         (cond ((string-match "freebsd" system-configuration) 'freebsd)
               ((string-match "openbsd" system-configuration) 'openbsd)
               ((string-match "netbsd"  system-configuration) 'netbsd))))))
  "Current operating system according to the format-all package.")

(defun +format--resolve-system (choices)
  "Get first choice matching `format-all-system-type' from CHOICES."
  (cl-loop for choice in choices
           if (atom choice) return choice
           else if (eql +format-system-type (car choice))
           return (cadr choice)))

;;;###autodef
(cl-defun set-formatter!
    (modes-or-name formatter
                   &key
                   name
                   modes
                   install
                   filter
                   ok-statuses
                   error-regexp)
  "Define a formatter.

MODES-OR-NAME can either be a major mode symbol (or list thereof), a unique name
for the formatter being defined (also a symbol), or a special list of
two-element sublists with the structure: (MAJOR-MODE FORM).

FORM is evaluated when the buffer is formatted and its return value serves two
purposes:

1. It is a predicate for this formatter. Assuming the MAJOR-MODE matches the
   current mode, if FORM evaluates to nil, the formatter is skipped.
2. It's return value is made available to FORMATTER if it is a function or list
   of shell arguments via the `mode-result' variable.

FORMATTER can be a function, string or nested list.

  If a function, it should be a formatter function that
    `format-all-buffer-thunk' will accept.
  If a string, it is assumed to be a shell command that the buffer's text will
    be piped to (through stdin).
  If a list, it should represent a shell command as a list of arguments. Each
    element is either a string or list (STRING ARG) where STRING is a format
    string and ARG is both a predicate and argument for STRING. If ARG is nil,
    STRING will be omitted from the vector.

NAME is a symbol that identifies this formatter. If NAME isn't specified and
FORMATTER is a function symbol, the symbol's name is used. If FORMATTER is a
string or list of strings, the executable is extracted from it and used as the
NAME. If FORMATTER is a lambda, NAME is required and will error if omitted. Note
that any formatters with the same NAME will be overwritten by FORMATTER.

INSTALL is a string representing the shell command to install this formatter's
dependencies. INSTALL can also be a two-element list: (OS COMMAND), or a list of
these. OS can be windows, macos, linux, freebsd, openbsd or netbsd.

FILTER is a function that takes three arguments: the formatted output, any error
output and the position of the first change. This function must return these
three after making whatever changes you like to them. This might be useful if
the output contains ANSI color codes that need to be stripped out (as is the
case with elm-format).

OK-STATUSES and ERROR-REGEXP are ignored if FORMATTER is not a shell command.

OK-STATUSES is a list of integer exit codes that should be treated as success
codes. However, if ERROR-REGEXP is given, and the program's stderr contains that
regexp, then the formatting is considered failed even if the exit status is in
OK-STATUSES.

Basic examples:

  (set-formatter! '(asm-mode nasm-mode) \"asmfmt\")
  (set-formatter! 'python-mode \"black -q -\" :install \"pip install black\")
  (set-formatter! 'tidy \"tidy -q -indent\" :modes '(html-mode web-mode))

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

  (set-formatter! 'html-tidy  ; overwrite predefined html-tidy formatter
    '(\"tidy\" \"-q\" \"-indent\"
      \"--tidy-mark\" \"no\"
      \"--drop-empty-elements\" \"no\"
      \"--show-body-only\" \"auto\"
      (\"--indent-spaces\" \"%d\" tab-width)
      (\"--indent-with-tabs\" \"%s\" (if indent-tabs-mode \"yes\" \"no\"))
      (\"-xml\" (memq major-mode '(nxml-mode xml-mode))))
    :ok-statuses '(0 1)))

  (set-formatter! 'elm-mode
    \"elm-format --yes --stdin\"
    :install '(macos \"brew install elm\")
    :filter
    (lambda (output errput first-diff)
      (list output
            (format-all-remove-ansi-color errput)
            first-diff)))"
  (declare (indent defun))
  (cl-check-type name (or symbol null))
  ;; Determine if MODES-OR-NAME means MODES or NAMES
  (if (and (symbolp modes-or-name)
           (not (string-match-p "-mode$" (symbol-name modes-or-name))))
      (setq name modes-or-name)
    (setq modes (doom-enlist modes-or-name)))
  (let* ((command-list (cond ((stringp formatter)   ; shell command
                              (split-string formatter " " t))
                             ((listp formatter)     ; shell command in lists
                              formatter)))
         (name (cond (name)
                     ((car command-list) (intern (car command-list)))
                     ((symbolp formatter) formatter)
                     ((user-error "Anonymous formatter requires a :name"))))
         (formatter
          (cond ((commandp formatter)
                 `(format-all-buffer-thunk
                   (lambda (input)
                     (with-silent-modifications
                       (setq buffer-file-name ,(buffer-file-name (buffer-base-buffer))
                             default-directory ,default-directory)
                       (delay-mode-hooks (funcall ',major-mode))
                       (insert input)
                       (condition-case e
                           (progn (call-interactively #',formatter)
                                  (list nil ""))
                         (error (list t (error-message-string e))))))))
                ((functionp formatter)
                 `(format-all-buffer-thunk #',formatter))
                (`(let (args)
                    (dolist (arg ',command-list)
                      (cond ((stringp arg) (push arg args))
                            ((listp arg)
                             (catch 'skip
                               (let (subargs this)
                                 (while (setq this (pop arg))
                                   (cond ((not (stringp (car arg)))
                                          (let ((val (eval (pop arg) t)))
                                            (unless val (throw 'skip nil))
                                            (push (format this val) subargs)))
                                         ((stringp this)
                                          (push this subargs))))
                                 (setq args (append subargs args)))))))
                    (if ,(and (or ok-statuses error-regexp) t)
                        (apply #'format-all-buffer-hard
                               ',ok-statuses ,error-regexp
                               (reverse args))
                      (apply #'format-all-buffer-easy (reverse args)))))))
         (fn
          `(lambda (executable mode-result)
             (let ((result ,formatter))
               ,(if filter
                    `(apply #',filter result)
                  'result))))
         (install (cond ((null install) install)
                        ((listp install)
                         (cdr (assq (+format--resolve-system) install)))
                        (install))))
    (after! format-all
      (puthash name (eval fn t) format-all-format-table)
      (puthash name (car command-list) format-all-executable-table)
      (puthash name (or install (gethash name format-all-install-table))
               format-all-install-table)
      (dolist (mode (doom-enlist modes))
        (cl-destructuring-bind (m &optional probe)
            (doom-enlist mode)
          (format-all-pushhash
           m (cons name (if probe `(lambda () ,probe)))
           format-all-mode-table))))
    name))
