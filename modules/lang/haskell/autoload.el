;;; lang/haskell/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +haskell/open-repl (&optional arg)
  "Opens a Haskell REPL."
  (interactive "P")
  (if-let*
      ((window
        (display-buffer
         (if (featurep! +intero)
             (intero-repl-buffer arg)
           (haskell-session-interactive-buffer (haskell-session))))))
      (window-buffer window)
    (error "Failed to display Haskell REPL")))
