;;; lang/sh/autoload.el -*- lexical-binding: t; -*-

(defvar sh-extra-font-lock--keywords
  `((+sh--match-var-in-double-quoted-string
     (2 font-lock-variable-name-face prepend))
    (,(concat
       "\\<"
       (regexp-opt '("sudo" "echo" "ls" "sleep" "tee" "cd" "cat" "service"))
       "\\>")
     (0 'font-lock-builtin-face))))

;;;###autoload
(defun +sh--in-double-quoted-string-p ()
  "Non-nil if point in inside a double-quoted string."
  (eq (nth 3 (syntax-ppss)) ?\"))

;;;###autoload
(defun +sh--match-var-in-double-quoted-string (limit)
  "Search for variables in double-quoted strings bounded by LIMIT."
  (let (res)
    (while
        (and (setq res
                   (re-search-forward
                    "\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)"
                    limit t))
             (not (+sh--in-double-quoted-string-p))))
    res))

;;;###autoload
(defun +sh|extra-fontify ()
  "Activate sh-extra-font-lock."
  (interactive)
  (font-lock-add-keywords nil sh-extra-font-lock--keywords)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

(defvar sh-shell-file)
;;;###autoload
(defun +sh/repl ()
  "Open a shell REPL."
  (let* ((dest-sh (symbol-name sh-shell))
         (sh-shell-file dest-sh))
    (sh-shell-process t)
    (with-current-buffer "*shell*"
      (rename-buffer (format "*shell [%s]*" dest-sh)))))
