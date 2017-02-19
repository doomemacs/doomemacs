;;; lang/html/autoload/evil.el

;;;###autoload (autoload '+html:encode-html-entities "lang/html/autoload/evil" nil t)
(evil-define-operator +html:encode-html-entities (beg end &optional input)
  "Encodes HTML entities in the selected region."
  (interactive "<r><a>")
  (cond (input
         (insert (+html-encode-entities input)))
        ((and beg end)
         (+html/encode-entities-region beg end))))

;;;###autoload (autoload '+html:decode-html-entities "lang/html/autoload/evil" nil t)
(evil-define-operator +html:decode-html-entities (beg end &optional input)
  "Decodes HTML entities in the selected region."
  (interactive "<r><a>")
  (cond (input
         (insert (+html-decode-entities input)))
        ((and beg end)
         (+html/decode-entities-region beg end))))

