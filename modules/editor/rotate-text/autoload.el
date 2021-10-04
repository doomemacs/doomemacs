;;; editor/rotate-text/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(with-eval-after-load 'rotate-text
  (pushnew! rotate-text-words
            '("true" "false")
            '("enable" "disable")))

;;;###autodef
(cl-defun set-rotate-patterns! (modes &key symbols words patterns)
  "Declare :symbols, :words or :patterns (all lists of strings) that
`rotate-text' will cycle through."
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (let ((fn-name (intern (format "+rotate-text-init-%s-h" mode))))
      (fset fn-name
            (lambda ()
              (setq-local rotate-text-local-symbols symbols)
              (setq-local rotate-text-local-words words)
              (setq-local rotate-text-local-patterns patterns)))
      (add-hook (intern (format "%s-hook" mode)) fn-name))))
