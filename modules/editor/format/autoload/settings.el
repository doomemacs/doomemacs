;;; editor/format/autoload/settings.el -*- lexical-binding: t; -*-

;; This must be redefined here because `format-all' only makes it available at
;; compile time.
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


(defun +format--make-command (formatter &rest _)
  `(format-all--buffer-thunk
    (lambda (input)
      (with-silent-modifications
        (setq buffer-file-name ,(buffer-file-name (buffer-base-buffer))
              default-directory ,default-directory)
        (delay-mode-hooks (funcall ',major-mode))
        (insert input)
        (condition-case e
            (progn
              (doom-log "formatter (commandp) %s" #',formatter)
              (call-interactively #',formatter)
              (list nil ""))
          (error (list t (error-message-string e))))))))

(defun +format--make-function (formatter &rest _)
  `(progn
     (doom-log "formatter (functionp) %s" #',formatter)
     (format-all--buffer-thunk #',formatter)))

(defun +format--make-shell-command (command ok-statuses error-regexp)
  (+format--make-shell-command-list (split-string command " " t)
                                    ok-statuses error-regexp))

(defun +format--make-shell-command-list (command-list ok-statuses error-regexp)
  `(let (args)
     (dolist (arg ',command-list)
       (cond ((stringp arg)
              (push arg args))
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
     (doom-log "formatter (arglist) %s" args)
     (if ,(and (or ok-statuses error-regexp) t)
         (apply #'format-all--buffer-hard
                ',ok-statuses ,error-regexp nil
                (reverse args))
       (apply #'format-all--buffer-easy (reverse args)))))

(cl-defun +format--set (name &key function modes unset)
  (declare (indent defun))
  (when (and unset (not (gethash name format-all--format-table)))
    (error "'%s' formatter does not exist to be unset" name))
  (puthash name function format-all--format-table)
  (dolist (mode (doom-enlist modes))
    (cl-destructuring-bind (m &optional probe)
        (doom-enlist mode)
      (if unset
          (puthash m (assq-delete-all name (gethash key format-all-mode-table))
                   format-all-mode-table)
        (format-all--pushhash
         m (cons name (if probe `(lambda () ,probe)))
         format-all--mode-table)))))

;;;###autodef
(cl-defun set-formatter!
    (name formatter &key modes filter ok-statuses error-regexp)
  "Define (or modify) a formatter named NAME.

Supported keywords: :modes :install :filter :ok-statuses :error-regexp

NAME is a symbol that identifies this formatter.

FORMATTER can be a symbol referring to another formatter, a function, string or
nested list.

  If a function, it should be a formatter function that
    `format-all--buffer-thunk' will accept.
  If a string, it is assumed to be a shell command that the buffer's text will
    be piped to (through stdin).
  If a list, it should represent a shell command as a list of arguments. Each
    element is either a string or list (STRING ARG) where STRING is a format
    string and ARG is both a predicate and argument for STRING. If ARG is nil,
    STRING will be omitted from the vector.

MODES is a major mode, a list thereof, or a list of two-element sublists with
the structure: (MAJOR-MODE FORM). FORM is evaluated when the buffer is formatted
and its return value serves two purposes:

  1. It is a predicate for this formatter. Assuming the MAJOR-MODE matches the
     current mode, if FORM evaluates to nil, the formatter is skipped.
  2. It's return value is made available to FORMATTER if it is a function or
     list of shell arguments via the `mode-result' variable.

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

  (set-formatter! 'asmfmt \"asmfmt\" :modes '(asm-mode nasm-mode))
  (set-formatter! 'black \"black -q -\")
  (set-formatter! 'html-tidy \"tidy -q -indent\" :modes '(html-mode web-mode))

Advanced examples:

  (set-formatter!
    'clang-format
    '(\"clang-format\"
      (\"-assume-filename=%S\" (or buffer-file-name mode-result \"\")))
    :modes
    '((c-mode \".c\")
      (c++-mode \".cpp\")
      (java-mode \".java\")
      (objc-mode \".m\")
      (protobuf-mode \".proto\")))

  (set-formatter! 'html-tidy
    '(\"tidy\" \"-q\" \"-indent\"
      (\"-xml\" (memq major-mode '(nxml-mode xml-mode))))
    :modes
    '(html-mode
      (web-mode (and (equal \"none\" web-mode-engine)
                     (car (member web-mode-content-type '(\"xml\" \"html\"))))))
    :ok-statuses '(0 1)
    :executable \"tidy\")

  (set-formatter! 'html-tidy  ; overwrite predefined html-tidy formatter
    '(\"tidy\" \"-q\" \"-indent\"
      \"--tidy-mark\" \"no\"
      \"--drop-empty-elements\" \"no\"
      \"--show-body-only\" \"auto\"
      (\"--indent-spaces\" \"%d\" tab-width)
      (\"--indent-with-tabs\" \"%s\" (if indent-tabs-mode \"yes\" \"no\"))
      (\"-xml\" (memq major-mode '(nxml-mode xml-mode))))
    :ok-statuses '(0 1)))

  (set-formatter! 'elm-format
    \"elm-format --yes --stdin\"
    :filter
    (lambda (output errput first-diff)
      (list output
            (format-all--remove-ansi-color errput)
            first-diff)))"
  (declare (indent defun))
  (cl-check-type name symbol)
  (after! format-all
    (if (null formatter)
        (+format--set name
          :unset t
          :modes modes)
      (let ((fn (funcall (cond ((stringp formatter)
                                #'+format--make-shell-command)
                               ((listp formatter)
                                #'+format--make-shell-command-list)
                               ((and (commandp formatter)
                                     (not (stringp formatter)))
                                #'+format--make-command)
                               ((functionp formatter)
                                #'+format--make-function))
                         formatter
                         ok-statuses
                         error-regexp)))
        (cl-check-type filter (or function null))
        (+format--set name
          :function
          `(lambda (executable mode-result)
             ,(if filter `(apply #',filter ,fn) fn))
          :modes modes)
        name))))
