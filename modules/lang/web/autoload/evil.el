;;; lang/html/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload (autoload '+web:encode-html-entities "lang/web/autoload/evil" nil t)
(evil-define-operator +web:encode-html-entities (beg end &optional bang input)
  "Encodes HTML entities in INPUT or the selected region."
  (interactive "<r><!><a>")
  (cond (input
         (let ((result (+web-encode-entities input)))
           (if bang
               (kill-new result)
             (insert result))))
        ((and beg end)
         (+web/encode-entities-region beg end))))

;;;###autoload (autoload '+web:decode-html-entities "lang/web/autoload/evil" nil t)
(evil-define-operator +web:decode-html-entities (beg end &optional bang input)
  "Decodes HTML entities in INPUT or the selected region."
  (interactive "<r><!><a>")
  (cond (input
         (let ((result (+web-decode-entities input)))
           (if bang
               (kill-new result)
             (insert result))))
        ((and beg end)
         (+web/decode-entities-region beg end))))
