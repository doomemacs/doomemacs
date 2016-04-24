;;; macros-editor.el

;;;###autoload
(defmacro def-electric! (modes &rest rest)
  (declare (indent 1))
  (let ((modes (if (listp modes) modes (list modes)))
        (chars (plist-get rest :chars))
        (words (plist-get rest :words)))
    (when (or chars words)
      (let ((fn-name (intern (format "narf--electric-%s"
                                     (s-join "-" (mapcar 'symbol-name modes))))))
        `(progn
           (defun ,fn-name ()
             (electric-indent-local-mode +1)
             ,(if chars `(setq electric-indent-chars ',chars))
             ,(if words `(setq narf-electric-indent-words ',words)))
           (add-hook! ,modes ',fn-name))))))

(provide 'macros-editor)
;;; macros-editor.el ends here
