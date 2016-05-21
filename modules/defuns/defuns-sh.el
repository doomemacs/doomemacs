;;; defuns-sh.el

(defvar sh-extra-font-lock--keywords
  `((doom/sh-extra-font-lock--match-var-in-double-quoted-string
     (2 font-lock-variable-name-face prepend))
    (,(concat
       "\\<"
       (regexp-opt '("sudo" "echo" "ls" "sleep" "tee" "cd" "cat" "service"))
       "\\>")
     (0 'font-lock-builtin-face))))

;;;###autoload
(defun doom/sh-extra-font-lock--is-in-double-quoted-string ()
  "Non-nil if point in inside a double-quoted string."
  (let ((state (syntax-ppss)))
    (eq (nth 3 state) ?\")))

;;;###autoload
(defun doom/sh-extra-font-lock--match-var-in-double-quoted-string (limit)
  "Search for variables in double-quoted strings bounded by LIMIT."
  (let (res)
    (while
        (and (setq res
                   (re-search-forward
                    "\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)"
                    limit t))
             (not (doom/sh-extra-font-lock--is-in-double-quoted-string))))
    res))

;;;###autoload
(defun doom|sh-extra-font-lock-activate ()
  "Activate sh-extra-font-lock."
  (interactive)
  (font-lock-add-keywords nil sh-extra-font-lock--keywords)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

;;;###autoload
(defun doom/inf-shell ()
  (let* ((dest-sh (symbol-name sh-shell))
         (sh-shell-file dest-sh))
    (sh-shell-process t)
    (with-current-buffer "*shell*"
      (rename-buffer (format "*shell [%s]*" dest-sh)))))

(provide 'defuns-sh)
;;; defuns-sh.el ends here
