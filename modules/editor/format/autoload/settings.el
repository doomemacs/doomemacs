;;; editor/format/autoload/settings.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-formatter! (name &rest rest)
  "Define (or modify) a formatter named NAME.

NAME is a symbol that identifies this formatter.

ARGS can be a symbol referring to another formatter, a function, string or
nested list.

  If a function, it should be a formatter function that
    `apheleia--run-formatter-function' will accept.
  If a string, it is assumed to be a shell command that the buffer's text will
    be piped to (through stdin).
  If a list, it should represent a shell command as a list of arguments. Each
    element is either a string or list (STRING ARG) where STRING is a format
    string and ARG is both a predicate and argument for STRING. If ARG is nil,
    STRING will be omitted from the vector.

If you're trying to override this, ensure that you wrap the call in `after!' and
whichever package sets the initial formatter. See the ':editor format' README
for more.

If ARGS isn't used, then the original definition of NAME is used (in
`apheleia-formatters'). Useful for setting what modes to use NAME in.

For more information on how to structure the list to be compatible, see
`apheleia--run-formatter-function'.

MODES is a major mode, a list thereof, or a list of two-element sublists with
the structure: (MAJOR-MODE FORM). FORM is evaluated when the buffer is formatted
and its return value serves two purposes:

  1. It is a predicate for this formatter. Assuming the MAJOR-MODE matches the
     current mode, if FORM evaluates to nil, the formatter is skipped.
  2. It's return value is made available to FORMATTER if it is a function or
     list of shell arguments via the `mode-result' variable.

Basic examples:
  (set-formatter! \\='asmfmt :modes \\='(asm-mode nasm-mode))
  (set-formatter! \\='black \"black -q -\")
  (set-formatter! \\='html-tidy \"tidy -q -indent\" :modes \\='(html-mode web-mode))

Advanced examples:
  (set-formatter!
    \\='clang-format
    \\='(\"clang-format\"
      (\"-assume-filename=%S\" (or buffer-file-name mode-result \"\")))
    :modes
    \\='((c-mode \".c\")
      (c++-mode \".cpp\")
      (java-mode \".java\")
      (objc-mode \".m\")
      (protobuf-mode \".proto\")))

  (set-formatter! \\='html-tidy
    \\='(\"tidy\" \"-q\" \"-indent\"
      (\"-xml\" (memq major-mode \\='(nxml-mode xml-mode))))
    :modes
    \\='(html-mode
      (web-mode (and (equal \"none\" web-mode-engine)
                     (car (member web-mode-content-type \\='(\"xml\" \"html\")))))))

  (set-formatter! \\='html-tidy  ; overwrite predefined html-tidy formatter
    \\='(\"tidy\" \"-q\" \"-indent\"
      \"--tidy-mark\" \"no\"
      \"--drop-empty-elements\" \"no\"
      \"--show-body-only\" \"auto\"
      (\"--indent-spaces\" \"%d\" tab-width)
      (\"--indent-with-tabs\" \"%s\" (if indent-tabs-mode \"yes\" \"no\"))
      (\"-xml\" (memq major-mode \\='(nxml-mode xml-mode)))))

  (set-formatter! \\='elm-format
    \"elm-format --yes --stdin\")

\(fn NAME [ARGS...] [:modes MODES])"
  (declare (indent defun))
  (cl-check-type name symbol)
  (after! apheleia
    (let* ((args (if (keywordp (car rest))
                     t
                   (pop rest)))
           (plist (cl-loop while (keywordp (car rest))
                           collect (pop rest)
                           collect (pop rest)))
           (modes (plist-get plist :modes)))
      (if (null args)
          (progn
            (setq apheleia-formatters
                  (assq-delete-all name apheleia-formatters))
            (while (rassoc name apheleia-mode-alist)
              (setq apheleia-mode-alist
                    (assq-delete-all (car (rassoc name apheleia-mode-alist)) apheleia-mode-alist))))
        (if (eq args t)
            (unless (assq name apheleia-formatters)
              (error "set-formatter!: unknown formatter (%s) cannot be assigned" name))
          (setf (alist-get name apheleia-formatters)
                (if (stringp args)
                    (ensure-list args)
                  args)))
        (dolist (mode (ensure-list modes))
          (setf (alist-get mode apheleia-mode-alist) name))))))
