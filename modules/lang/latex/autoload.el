;;; lang/latex/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +latex-indent-item-fn ()
  "Indent LaTeX \"itemize\",\"enumerate\", and \"description\" environments.

\"\\item\" is indented `LaTeX-indent-level' spaces relative to the beginning
of the environment.

See `LaTeX-indent-level-item-continuation' for the indentation strategy this
function uses."
  (save-match-data
    (let* ((re-beg "\\\\begin{")
           (re-end "\\\\end{")
           (re-env "\\(?:itemize\\|\\enumerate\\|description\\)")
           (indent (save-excursion
                     (when (looking-at (concat re-beg re-env "}"))
                       (end-of-line))
                     (LaTeX-find-matching-begin)
                     (+ LaTeX-item-indent (current-column))))
           (contin (pcase +latex-indent-item-continuation-offset
                     (`auto LaTeX-indent-level)
                     (`align 6)
                     (`nil (- LaTeX-indent-level))
                     (x x))))
      (cond ((looking-at (concat re-beg re-env "}"))
             (or (save-excursion
                   (beginning-of-line)
                   (ignore-errors
                     (LaTeX-find-matching-begin)
                     (+ (current-column)
                        LaTeX-item-indent
                        LaTeX-indent-level
                        (if (looking-at (concat re-beg re-env "}"))
                            contin
                          0))))
                 indent))
            ((looking-at (concat re-end re-env "}"))
             (save-excursion
               (beginning-of-line)
               (ignore-errors
                 (LaTeX-find-matching-begin)
                 (current-column))))
            ((looking-at "\\\\item")
             (+ LaTeX-indent-level indent))
            ((+ contin LaTeX-indent-level indent))))))

;;;###autoload
(defun +latex-fold-last-macro-a (&rest _)
  "Advice to auto-fold LaTeX macros after functions that
typically insert macros."
  ;; A simpler approach would be to just fold the whole line, but if point was
  ;; inside a macro that would kick it out. So instead we fold the last macro
  ;; before point, hoping its the one newly inserted.
  (TeX-fold-region (save-excursion
                     (search-backward "\\" (line-beginning-position) t)
                     (point))
                   (1+ (point))))

;;;###autoload
(defun +latex-symbols-company-backend (command &optional arg &rest _ignored)
  "A wrapper backend for `company-mode' that either uses
`company-math-symbols-unicode' or `company-math-symbols-latex'. If
`+latex-enable-unicode-math' is non-nil use the former, otherwise the latter."
  (if +latex-enable-unicode-math
      (company-math-symbols-unicode command arg)
    (company-math-symbols-latex command arg)))
