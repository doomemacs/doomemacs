;;; lisp/cli/info.el --- information about your Doom install -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Variables

;; None yet!


;;
;;; Commands

(defcli! info
    ((format ("--lisp" "--json") "What format to dump info into")
     &context context)
  "Print detailed information about your config for bug reports."
  (with-temp-buffer
    (pcase format
      ("--json"
       (require 'json)
       (insert (json-encode (doom-info)))
       (json-pretty-print-buffer))
      ("--lisp"
       (pp (doom-info)))
      (_
       (insert (doom-info-string
                (if (doom-cli-context-pipe-p context :out t)
                    72
                  (doom-cli-context-width context))))))
    (print! "%s" (string-trim-right (buffer-string)))))

(provide 'doom-cli-info)
;;; info.el ends here
