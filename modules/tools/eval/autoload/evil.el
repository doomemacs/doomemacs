;; tools/eval/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload (autoload '+eval:region "tools/eval/autoload/evil" nil t)
(evil-define-operator +eval:region (beg end)
  "Evaluate selection or sends it to the open REPL, if available."
  :move-point nil
  (interactive "<r>")
  (if (and (fboundp '+eval--ensure-in-repl-buffer)
           (+eval--ensure-in-repl-buffer))
      (+eval/send-region-to-repl (point-min) (point-max))
    (+eval/region beg end)))

;;;###autoload (autoload '+eval:replace-region "tools/eval/autoload/evil" nil t)
(evil-define-operator +eval:replace-region (beg end)
  "Evaluate selection and replace it with its result."
  :move-point nil
  (interactive "<r>")
  (+eval/region-and-replace beg end))

;;;###autoload (autoload '+eval:repl "tools/eval/autoload/evil" nil t)
(evil-define-operator +eval:repl (beg end &optional bang)
  "Open REPL and send the current selection to it."
  :move-point nil
  (interactive "<r><!>")
  (+eval/open-repl-other-window))
