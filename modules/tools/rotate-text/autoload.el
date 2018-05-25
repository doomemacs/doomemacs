;;; tools/rotate-text/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(def-setting! :rotate (modes &rest plist)
  "Declare :symbols, :words or :patterns (all lists of strings) that
`rotate-text' will cycle through."
  (declare (indent 1))
  (let* ((modes (doom-enlist (doom-unquote modes)))
         (fn-name (intern (format "doom--rotate-%s" (mapconcat #'symbol-name modes "-")))))
    `(progn
       (defun ,fn-name ()
         (require 'rotate-text)
         (let ((plist (list ,@plist)))
           (setq rotate-text-local-symbols  (plist-get plist :symbols)
                 rotate-text-local-words    (plist-get plist :words)
                 rotate-text-local-patterns (plist-get plist :patterns))))
       (add-hook! ,modes #',fn-name))))
