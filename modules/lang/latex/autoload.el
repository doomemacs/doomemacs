;;; lang/latex/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(def-setting! :latex-bibtex-file (file)
  "Sets the default file RefTeX uses to search for citations."
  `(setq +latex-bibtex-file ,file))

;;;###autoload
(def-setting! :latex-bibtex-dir (dir)
  "Sets the directory where AUCTeX will search for PDFs associated to BibTeX references."
  `(setq +latex-bibtex-dir ,dir))

;;;###autoload
(defun +latex/LaTeX-indent-item ()
  "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and \"description\" environments.

  \"\\item\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `LaTeX-indent-level-item-continuation'
  if the latter is bound."
  (save-match-data
    (let* ((offset LaTeX-indent-level)
           (contin (or (and (boundp '+latex-indent-level-item-continuation)
                            +latex-indent-level-item-continuation)
                       (* 4 LaTeX-indent-level)))
           (re-beg "\\\\begin{")
           (re-end "\\\\end{")
           (re-env "\\(itemize\\|\\enumerate\\|description\\)")
           (indent (save-excursion
                     (when (looking-at (concat re-beg re-env "}"))
                       (end-of-line))
                     (LaTeX-find-matching-begin)
                     (current-column))))
      (cond ((looking-at (concat re-beg re-env "}"))
             (or (save-excursion
                   (beginning-of-line)
                   (ignore-errors
                     (LaTeX-find-matching-begin)
                     (+ (current-column)
                        (if (looking-at (concat re-beg re-env "}"))
                            contin
                          offset))))
                 indent))
            ((looking-at (concat re-end re-env "}"))
             indent)
            ((looking-at "\\\\item")
             (+ offset indent))
            (t
             (+ contin indent))))))


