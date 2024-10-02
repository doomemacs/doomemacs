;;; lisp/lib/strings.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-pcre-quote (str)
  "Like `reqexp-quote', but for PCREs."
  (let ((special '(?. ?^ ?$ ?* ?+ ?? ?{ ?\\ ?\[ ?\| ?\())
        (quoted nil))
    (mapc (lambda (c)
            (when (memq c special)
              (push ?\\ quoted))
            (push c quoted))
          str)
    (concat (nreverse quoted))))

(provide 'doom-lib '(strings))
;;; strings.el ends here
