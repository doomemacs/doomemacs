;;; core/cli/debug.el -*- lexical-binding: t; -*-

(load! "autoload/debug" doom-core-dir)


;;
;;; Commands

(def-command! info (&optional format)
  "Output system info in markdown for bug reports."
  (pcase format
    ("json"
     (require 'json)
     (with-temp-buffer
       (insert (json-encode (doom-info)))
       (json-pretty-print-buffer)
       (print! (buffer-string))))
    ((or "md" "markdown")
     (doom/info))
    (_ (doom/info 'raw)))
  nil)

(def-command! (version v) ()
  "Reports the version of Doom and Emacs."
  (doom/version)
  nil)
