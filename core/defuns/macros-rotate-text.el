;;; macros-rotate-text.el

;;;###autoload
(defmacro def-rotate! (modes &rest rest)
  (declare (indent 1))
  (let ((modes (if (listp modes) modes (list modes)))
        (symbols (plist-get rest :symbols))
        (words (plist-get rest :words))
        (patterns (plist-get rest :patterns))
        fn-name)
    (setq fn-name (intern (format "narf--rotate-%s"
                                  (s-join "-" (mapcar 'symbol-name modes)))))
    `(progn
       (defun ,fn-name ()
         ,(when symbols
            `(setq-local rotate-text-local-symbols ',symbols))
         ,(when words
            `(setq-local rotate-text-local-words ',words))
         ,(when patterns
            `(setq-local rotate-text-local-patterns ',patterns)))
       (add-hook! ,modes ',fn-name))))

(provide 'macros-rotate-text)
;;; macros-rotate-text.el ends here
