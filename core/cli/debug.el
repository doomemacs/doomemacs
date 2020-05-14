;;; core/cli/debug.el -*- lexical-binding: t; -*-

(load! "autoload/debug" doom-core-dir)

;;
;;; Commands

(defcli! info
    ((format ["--json" "--md" "--lisp"] "What format to dump info into"))
  "Output system info in markdown for bug reports."
  (pcase format
    ("--json"
     (require 'json)
     (with-temp-buffer
       (insert (json-encode (doom-info)))
       (json-pretty-print-buffer)
       (print! (buffer-string))))
    ("--md"
     (doom/info))
    ((or `nil "--lisp")
     (doom/info 'raw))
    (_
     (user-error "I don't understand %S. Did you mean --json, --md/--markdown or --lisp?"
                 format)))
  nil)

(defcli! (version v) ()
  "Show version information for Doom & Emacs."
  (doom/version)
  nil)
