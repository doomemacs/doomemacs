;;; defuns-scss.el

;;;###autoload
(defun narf/scss-toggle-inline-or-block ()
  "Toggles between a SCSS multiline block and one-line block."
  (interactive)
  (save-excursion
    (let* ((bounds (ignore-errors (evil-a-curly)))
           (beg (car bounds))
           (end (cadr bounds)))
      (goto-char beg)
      (unless bounds
        (user-error "No block found"))
      (if (= (line-number-at-pos beg) (line-number-at-pos end))
          (progn (replace-regexp ";[\s\t]*" ";\n" nil beg end)
                 (save-excursion (goto-char (1+ beg)) (insert "\n")
                                 (evil-indent beg (+ 2 end))))
        (evil-join beg end)))))

(provide 'defuns-scss)
;;; defuns-scss.el ends here
