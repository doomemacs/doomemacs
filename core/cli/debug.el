;;; core/cli/debug.el -*- lexical-binding: t; -*-

(load! "autoload/debug" doom-core-dir)


;;
;;; Commands

(defcli! info (&optional format)
  "Output system info in markdown for bug reports.

Will print in the following formats:

  --json
  --md / --markdown
  --lisp

If no arguments are given, --raw is assumed."
  (pcase format
    ("--json"
     (require 'json)
     (with-temp-buffer
       (insert (json-encode (doom-info)))
       (json-pretty-print-buffer)
       (print! (buffer-string))))
    ((or "--md" "--markdown")
     (doom/info))
    ((or `nil "--lisp")
     (doom/info 'raw))
    (_
     (user-error "I don't understand %S. Did you mean --json, --md/--markdown or --lisp?"
                 format)))
  nil)

(defcli! (version v) ()
  "Reports the version of Doom and Emacs."
  (doom/version)
  nil)
