;;; lang/sh/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +sh--match-variables-in-quotes (limit)
  "Search for variables in double-quoted strings bounded by LIMIT."
  (with-syntax-table sh-mode-syntax-table
    (let (res)
      (while
          (and (setq res
                     (re-search-forward
                      "\\(\\$\\)\\({.+?}\\|\\<.+?\\>\\)"
                      limit t))
               (not (eq (nth 3 (syntax-ppss)) ?\"))))
      res)))

;;;###autoload
(defun +sh--match-command-subst-in-quotes (limit)
  "Search for variables in double-quoted strings bounded by LIMIT."
  (with-syntax-table sh-mode-syntax-table
    (let (res)
      (while
          (and (setq res
                     (re-search-forward "\\(\\$(.+?)\\|`.+?`\\)"
                                        limit t))
               (not (eq (nth 3 (syntax-ppss)) ?\"))))
      res)))

(defvar sh-shell-file)
;;;###autoload
(defun +sh/repl ()
  "Open a shell REPL."
  (let* ((dest-sh (symbol-name sh-shell))
         (sh-shell-file dest-sh))
    (sh-shell-process t)
    (with-current-buffer "*shell*"
      (rename-buffer (format "*shell [%s]*" dest-sh)))))
