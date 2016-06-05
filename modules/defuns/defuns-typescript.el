;;; defuns-typescript.el

(defvar ts-externs-vars-re
  (concat "\\_<"
          (regexp-opt
           '("__dirname" "__filename"))
          "\\_>"))

(defvar ts-externs-fns-re
  (concat "\\_<"
          (regexp-opt
           '("Buffer" "clearInterval" "clearTimeout" "require" "setInterval"
             "setTimeout" "querystring" "setImmediate" "clearImmediate"))
          "\\_>"))

(defvar ts-ecma-externs-re
  (concat "\\_<"
          (regexp-opt '("decoreURI" "decoreURIComponent" "encodeURI"
                        "encodeURIComponent" "escape" "eval" "isFinite"
                        "isNaN" "parseFloat" "parseInt" "escape" "unescape"))
          "\\_>"))

(defface ts-object-property '((t (:inherit font-lock-function-name-face)))
  "")

;;;###autoload
(defun doom|ts-fontify ()
  (font-lock-add-keywords
   nil `((,ts-externs-vars-re
          . 'font-lock-builtin-face)
         (,ts-externs-fns-re
          . 'font-lock-keyword-face)
         (,ts-ecma-externs-re
          . 'font-lock-builtin-face)
         ;; Lambda character
         (" \\(=>\\) "
          . 'font-lock-preprocessor-face)
         ;; $-prefixed variables
         ("\\_<\\$[[:alnum:]_]+\\_>"
          . 'font-lock-keyword-face)
         ;; object keys
         ("\\(?:^\\|,\\|{\\)\\s-*\\([[:alpha:]_$][[:alnum:]_$]*\\)\\s-*:"
          1 'ts-object-property)
         ;; variable.prefixes
         ("\\_<\\([a-z_$][[:alnum:]_$]*\\)\\."
          1 'font-lock-variable-name-face)
         ;; functioncalls()
         ("\\_<\\([a-z_$][[:alnum:]_$]*\\)("
          1 'font-lock-function-name-face)
         ;; PascalCase ClassNames
         ("\\_<[A-Z][a-z0-9_]+\\_>"
          . 'font-lock-type-face)
         ;; CONSTANTS
         ("\\_<[A-Z0-9_]+\\_>"
          . 'font-lock-builtin-face)
         ;; Import froms
         ("^\\s-*import\\s-+\\(?:{[^}]*}\\|[^ ]+\\|[^ ]+\\s-+as\\s-+[^ ]+\\)\\s-+\\(from\\)\\s-"
          (1 'font-lock-keyword-face))
         ("^\\s-*import\\s-+\\(?:[^ ]+\\s-+\\(as\\)\\s-+[^ ]+\\)\\s-+from\\s-"
          (1 'font-lock-keyword-face))

         ;; ES6 Lambda parameters (...) => {}
         (,(concat
            "\\s-(\\s-*"
            typescript--name-start-re)
          ,(list (concat "\\(" typescript--name-re "\\)\\(\\s-*).*\\)?\\s-*")
                 '(backward-char)
                 '(end-of-line)
                 '(1 font-lock-variable-name-face)))
         )))

(provide 'defuns-typescript)
;;; defuns-typescript.el ends here
