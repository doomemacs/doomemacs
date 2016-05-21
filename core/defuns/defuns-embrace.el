;;; defuns-embrace.el

(defun doom--embrace-get-pair (char)
  (acond ((cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))
          `(,(car it) . ,(cdr it)))
         ((assoc-default char embrace--pairs-list)
          (if (functionp (embrace-pair-struct-read-function it))
              (let ((pair (funcall (embrace-pair-struct-read-function it))))
                `(,(car pair) . ,(cdr pair)))
            `(,(embrace-pair-struct-left it) . ,(embrace-pair-struct-right it))))
         (t `(,char . ,char))))

;;;###autoload
(defun doom/embrace-escaped ()
  "Escaped surround characters."
  (let ((char (read-char "\\")))
    (if (eq char 27)
        (cons "" "")
      (let ((pair (doom--embrace-get-pair (string char)))
            (text (if (sp-point-in-string) "\\\\%s" "\\%s")))
        (cons (format text (car pair))
              (format text (cdr pair)))))))

;;;###autoload
(defun doom/embrace-latex ()
  "LaTeX commands"
  (cons (format "\\%s{" (read-string "\\")) "}"))

;;;###autoload
(defun doom/embrace-elisp-fn ()
  (cons (format "(%s " (or (read-string "(") "")) ")"))

(provide 'defuns-embrace)
;;; defuns-embrace.el ends here
