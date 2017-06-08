;;; feature/eval/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+eval:region "feature/eval/autoload/evil" nil t)
(evil-define-operator +eval:region (beg end)
  "Send region to the currently open repl, if available."
  :move-point nil
  (interactive "<r>")
  (+eval/region beg end))

;;;###autoload (autoload '+eval:replace-region "feature/eval/autoload/evil" nil t)
(evil-define-operator +eval:replace-region (beg end)
  :move-point nil
  (interactive "<r>")
  (+eval/region-and-replace beg end))

;;;###autoload (autoload '+eval:repl "feature/eval/autoload/evil" nil t)
(evil-define-operator +eval:repl (beg end &optional bang)
  :move-point nil
  (interactive "<r><!>")
  (if (evil-normal-state-p)
      (+eval/repl)
    (+eval/repl-send-region beg end bang)))
