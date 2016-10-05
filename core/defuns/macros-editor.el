;;; macros-editor.el

;;;###autoload
(defmacro def-electric! (modes &rest rest)
  "Declare :words (list of strings) or :chars (lists of chars) in MODES that
trigger electric indentation."
  (declare (indent 1))
  (let ((modes (-list modes))
        (chars (plist-get rest :chars))
        (words (plist-get rest :words)))
    (when (or chars words)
      (let ((fn-name (intern (format "doom--electric-%s" (s-join "-" (mapcar 'symbol-name modes))))))
        `(progn
           (defun ,fn-name ()
             (electric-indent-local-mode +1)
             ,(if chars `(setq electric-indent-chars ',chars))
             ,(if words `(setq doom-electric-indent-words ',words)))
           (add-hook! ,modes ',fn-name))))))

;;;###autoload
(defmacro def-rotate! (modes &rest rest)
  "Declare :symbols, :words or :patterns that `rotate-text' will cycle through."
  (declare (indent 1))
  (let ((modes (if (listp modes) modes (list modes)))
        (symbols (plist-get rest :symbols))
        (words (plist-get rest :words))
        (patterns (plist-get rest :patterns)))
    (when (or symbols words patterns)
      (let ((fn-name (intern (format "doom--rotate-%s" (s-join "-" (mapcar 'symbol-name modes))))))
        `(progn
           (defun ,fn-name ()
             ,(if symbols `(setq-local rotate-text-local-symbols ',symbols))
             ,(if words `(setq-local rotate-text-local-words ',words))
             ,(if patterns `(setq-local rotate-text-local-patterns ',patterns)))
           (add-hook! ,modes ',fn-name))))))

(provide 'macros-editor)
;;; macros-editor.el ends here
