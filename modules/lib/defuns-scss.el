;;; defuns-scss.el

;;;###autoload
(defun narf/scss-toggle-inline-or-block ()
  "Toggles between a SCSS multiline block and one-line block."
  (interactive)
  (save-excursion
    (let* ((bounds (ignore-errors (evil-a-curly)))
           beg end)
      (unless bounds
        (user-error "No block found"))
      (setq beg (car bounds))
      (setq end (cadr bounds))
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

(provide 'defuns-scss)
;;; defuns-scss.el ends here
