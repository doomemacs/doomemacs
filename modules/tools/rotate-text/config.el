;;; tools/rotate-text/config.el

(def-package! rotate-text
  :commands (rotate-text rotate-text-backward)
  :config
  (push '("true" "false") rotate-text-words))


(def-setting! :rotate (modes &rest plist)
  "Declare :symbols, :words or :patterns that `rotate-text' will cycle through."
  (declare (indent 1))
  (let ((modes (if (listp modes) modes (list modes)))
        (symbols  (plist-get plist :symbols))
        (words    (plist-get plist :words))
        (patterns (plist-get plist :patterns)))
    (when (or symbols words patterns)
      (let ((fn-name (intern (format "doom--rotate-%s" (s-join "-" (mapcar 'symbol-name modes))))))
        `(progn
           (defun ,fn-name ()
             (require 'rotate-text)
             ,(if symbols `(setq rotate-text-local-symbols ',symbols))
             ,(if words `(setq rotate-text-local-words ',words))
             ,(if patterns `(setq rotate-text-local-patterns ',patterns)))
           (add-hook! ,modes ',fn-name))))))

