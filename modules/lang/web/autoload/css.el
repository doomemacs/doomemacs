;;; lang/web/+css.el

;;;###autoload
(defun +css/scss-build ()) ; TODO

;;;###autoload
(defun +css/sass-build ()) ; TODO

;;;###autoload
(defun +css/toggle-inline-or-block ()
  "Toggles between a bracketed block and inline block."
  (interactive)
  ;; TODO Remove evil dependency
  (save-excursion
    (let* ((bounds (or (ignore-errors (evil-a-curly))
                       (user-error "No block found")))
           (beg (car bounds))
           (end (cadr bounds)))
      (if (= (line-number-at-pos beg) (line-number-at-pos end))
          (save-excursion
            (goto-char (1+ beg)) (insert "\n")
            (unless (string-match ";[\s\t]*}$" (buffer-substring-no-properties beg (1+ end)))
              (goto-char end) (insert "\n"))
            (replace-regexp ";[\s\t]*" ";\n" nil beg (1+ end))
            (setq end (cadr (evil-a-curly)))
            (evil-indent beg end)
            (delete-trailing-whitespace beg end))
        (goto-char beg)
        (evil-join beg end)
        (goto-char (1+ beg))
        (just-one-space)
        (goto-char (cadr (evil-inner-curly)))
        (just-one-space)))))
