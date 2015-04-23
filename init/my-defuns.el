
(defmacro -defreplace (name search replace)
  `(evil-define-operator ,(make-symbol (concat "replace:" (symbol-name name))) (beg end)
    :type inclusive
    :repeat nil
    (interactive "<r>")
    (replace-regexp ,search ,replace t beg end)))

;;;; HTML ;;;;
;; Replace smart quotes and other MS Word verbiage into plain text
(defun replace:plain-textify (beg end)
  (interactive "r")
  (replace-regexp "…" "..."   nil beg end)
  (replace-regexp "[‘’]" "'"  nil beg end)
  (replace-regexp "[“”]" "\"" nil beg end))

;; Email address with mailto link
(defun replace:email2mailto (beg end)
  (interactive "r")
  (replace-regexp "\\b\\([a-zA-Z0-9._+-%]+@[a-zA-Z0-9-.]+\\.[a-zA-Z]+\\)\\b"
                  "<a href=\"mailto:\\1\">\\1</a>"
                  nil beg end))

;; Link with anchor
(defun replace:url2anchor (beg end)
  (interactive "r")
  (replace-regexp "\\bhttps?://.+?\\b"
                  "<a href=\"\\1\">\\1</a>"
                  nil beg end))


(provide 'my-defuns)
;;; my-defuns.el ends here
