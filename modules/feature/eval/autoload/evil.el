;;; feature/eval/autoload/evil.el

;;;###autoload (autoload '+repl:eval-region "feature/eval/autoload/evil" nil t)
(evil-define-operator +repl:eval-region (beg end)
  (interactive "<r>")
  (+repl/eval-region beg end))

;;;###autoload (autoload '+repl:eval-region-and-replace "feature/eval/autoload/evil" nil t)
(evil-define-operator +repl:eval-region-and-replace (beg end)
  (interactive "<r>")
  (+repl/eval-region-and-replace beg end))

