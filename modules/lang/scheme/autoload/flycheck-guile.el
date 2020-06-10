;;; flycheck-guile --- Checker for guile using guild -*- lexical-binding: t; -*-

;; NOTE: This checker was initially borrowed from guile-studio:
;; https://git.elephly.net/software/guile-studio.git

(defvar flycheck-guile-warnings
  '(;"unsupported-warning"         ; warn about unknown warning types
    "unused-variable"             ; report unused variables
    "unused-toplevel"             ; report unused local top-level variables
    ;"shadowed-toplevel"           ; report shadowed top-level variables
    "unbound-variable"            ; report possibly unbound variables
    "macro-use-before-definition" ; report possibly mis-use of macros before they are defined
    "arity-mismatch"              ; report procedure arity mismatches (wrong number of arguments)
    "duplicate-case-datum"        ; report a duplicate datum in a case expression
    "bad-case-datum"              ; report a case datum that cannot be meaningfully compared using `eqv?'
    "format"                      ; report wrong number of arguments to `format'
    )
  "A list of warnings to enable for `guild compile'.

The value of this variable is a list of strings, where each string names a
supported warning type.

The list of supported warning types can be found by running
`guild compile -W help'.")

(flycheck-define-checker guile
  "A Guile syntax checker using `guild compile'."
  :command ("guild" "compile" "--to=cps"
            (option-list "-W" flycheck-guile-warnings)
            (option-list "-L" geiser-guile-load-path list expand-file-name)
            source)
  :predicate
  (lambda ()
    (and (boundp 'geiser-impl--implementation)
         (eq geiser-impl--implementation 'guile)))
  :verify
  (lambda (checker)
    (let ((geiser-impl (bound-and-true-p geiser-impl--implementation)))
      (list
       (flycheck-verification-result-new
        :label "Geiser Implementation"
        :message (cond
                  ((eq geiser-impl 'guile) "Guile")
                  (geiser-impl (format "Other: %s" geiser-impl))
                  (t "Geiser not active"))
        :face (cond
               ((or (eq geiser-impl 'guile)) 'success)
               (t '(bold error)))))))
  :error-patterns
  ((warning
    line-start
    (file-name) ":" line ":" column ": warning:" (message) line-end)
   (error
    line-start
    "ice-9/boot-9.scm:" (+ digit) ":" (+ digit) ":" (+ (any space "\n"))
    "In procedure raise-exception:"                 (+ (any space "\n"))
    "In procedure " (id (+ (not ":"))) ":"          (+ (any space "\n"))
    (file-name) ":" line ":" column ":" (message (+? anything)) (* space) string-end)
   (error
    line-start
    "ice-9/boot-9.scm:" (+ digit) ":" (+ digit) ":" (+ (any space "\n"))
    "In procedure raise-exception:"                 (+ (any space "\n"))
    (id (+ (not ":"))) ":"                          (+ (any space "\n"))
    (file-name) ":" line ":" column ":" (message (+? anything)) (* space) string-end)
   (error
    line-start
    (file-name) ":" line ":" column ":" (message (+? anything)) (* space) string-end)
   )
  :modes (scheme-mode geiser-mode))

(add-to-list 'flycheck-checkers 'guile)
