;;; extra-text.el

;; TODO
(evil-define-operator narf/html-encode (beg end)
  "HTML encode the selected region."
  (interactive "<r>")
  (shell-command-on-region beg end "sort" nil t))

(provide 'extra-text)
;;; extra-text.el ends here
