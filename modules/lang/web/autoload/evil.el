;;; lang/html/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload (autoload '+web:encode-html-entities "lang/web/autoload/evil" nil t)
(evil-define-operator +web:encode-html-entities (beg end &optional input)
  "Encodes HTML entities in the selected region."
  (interactive "<r><a>")
  (cond (input
         (insert (+web-encode-entities input)))
        ((and beg end)
         (+web/encode-entities-region beg end))))

;;;###autoload (autoload '+web:decode-html-entities "lang/web/autoload/evil" nil t)
(evil-define-operator +web:decode-html-entities (beg end &optional input)
  "Decodes HTML entities in the selected region."
  (interactive "<r><a>")
  (cond (input
         (insert (+web-decode-entities input)))
        ((and beg end)
         (+web/decode-entities-region beg end))))

