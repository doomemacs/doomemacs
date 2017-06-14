;;; core/autoload/test.el -*- lexical-binding: t; -*-

;;;###autoload
(defmacro def-test! (name &rest body)
  "Define a namespaced ERT test."
  (declare (indent defun) (doc-string 2))
  (unless (plist-get body :disabled)
    `(ert-deftest
         ,(cl-loop with path = (file-relative-name (file-name-sans-extension load-file-name)
                                                   doom-emacs-dir)
                   for (rep . with) in '(("/test/" . "/") ("/" . ":"))
                   do (setq path (replace-regexp-in-string rep with path t t))
                   finally return (intern (format "%s::%s" path name))) ()
       ()
       ,@body)))
