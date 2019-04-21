;; tools/eval/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload (autoload '+eval:region "tools/eval/autoload/evil" nil t)
(evil-define-operator +eval:region (beg end)
  "Send region to the currently open repl, if available."
  :move-point nil
  (interactive "<r>")
  (+eval/region beg end))

;;;###autoload (autoload '+eval:replace-region "tools/eval/autoload/evil" nil t)
(evil-define-operator +eval:replace-region (beg end)
  :move-point nil
  (interactive "<r>")
  (+eval/region-and-replace beg end))

;;;###autoload (autoload '+eval:repl "tools/eval/autoload/evil" nil t)
(evil-define-operator +eval:repl (beg end &optional bang)
  :move-point nil
  (interactive "<r><!>")
  (if (evil-normal-state-p)
      (+eval/open-repl-other-window bang)
    (+eval/send-region-to-repl beg end bang)))
