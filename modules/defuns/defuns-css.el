;;; defuns-scss.el

;;;###autoload
(defun narf/css-toggle-inline-or-block ()
  "Toggles between a SCSS multiline block and one-line block."
  (interactive)
  (save-excursion
    (let* ((bounds (ignore-errors (evil-a-curly)))
           beg end)
      (unless bounds
        (user-error "No block found"))
      (setq beg (car bounds)
            end (cadr bounds))
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

;;;###autoload
(defalias 'narf/sass-build 'narf/scss-build)

;;;###autoload
(defun narf/scss-build ()
  "Compile all sass/scss files in project"
  (interactive)
  (let ((scss-dir (f-slash (or (f-traverse-upwards (lambda (d)
                                                     (string-match-p "/\\(\\(s[ca]\\|c\\)ss\\|styles\\)/?$" d))
                                                   default-directory)
                               default-directory))))
    (compile (format "%s %s --update '%s':'%s'"
                     scss-sass-command
                     (mapconcat 'identity scss-sass-options " ")
                     scss-dir
                     (or scss-output-directory
                         (awhen (f-traverse-upwards (lambda (d)
                                                      (f-dir? (format "%s/css" d)))
                                                    default-directory)
                           (format "%s/css" it))
                         ".")))))

(provide 'defuns-scss)
;;; defuns-scss.el ends here
