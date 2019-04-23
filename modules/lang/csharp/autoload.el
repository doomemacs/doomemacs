;;; lang/csharp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +csharp-sp-point-in-type-p (id action context)
  "Return t if point is in the right place for C# angle-brackets."
  (and (sp-in-code-p id action context)
       (cond ((eq action 'insert)
              (sp-point-after-word-p id action context))
             ((eq action 'autoskip)
              (/= (char-before) 32)))))
