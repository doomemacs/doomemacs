;;; lang/web/autoload/css.el -*- lexical-binding: t; -*-

;;;###autoload
;; TODO (defun +css/scss-build ())

;;;###autoload
;; TODO (defun +css/sass-build ())

;;;###autoload
(defun +css/toggle-inline-or-block ()
  "Toggles between a bracketed block and inline block."
  (interactive)
  (let ((inhibit-modification-hooks t))
    (cl-destructuring-bind (&key beg end op cl &allow-other-keys)
        (sp-get-thing)
      (when (or (string-empty-p op) (string-empty-p cl))
        (user-error "No block found"))
      (with-no-warnings
        (if (= (line-number-at-pos beg) (line-number-at-pos end))
            (progn
              (goto-char end) (insert "\n")
              (goto-char (1+ beg)) (insert "\n")
              (replace-regexp ";\\s-+" ";\n" nil beg end)
              (indent-region beg end))
          (replace-regexp "\n" " " nil beg end)
          (replace-regexp " +" " " nil beg end))))))
