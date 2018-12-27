;;; lang/haskell/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +haskell-repl-buffer (&optional arg)
  "Returns the appropriate Haskell REPL buffer."
  (if (featurep! +intero)
      (intero-repl-buffer arg)
    (haskell-session-interactive-buffer (haskell-session))))

;;;###autoload
(defun +haskell/repl (&optional arg)
  "Opens a Haskell REPL."
  (interactive "P")
  (display-buffer (+haskell-repl-buffer arg)))
