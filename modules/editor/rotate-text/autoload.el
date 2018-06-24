;;; editor/rotate-text/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(after! rotate-text
  (add-to-list 'rotate-text-words '("true" "false")))

;;;###autodef
(cl-defun set-rotate-patterns! (modes &key symbols words patterns)
  "Declare :symbols, :words or :patterns (all lists of strings) that
`rotate-text' will cycle through."
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (let ((fn-name (intern (format "+rotate-text|init-%s" mode))))
      (fset fn-name
            (lambda ()
              (setq-local rotate-text-local-symbols symbols)
              (setq-local rotate-text-local-words words)
              (setq-local rotate-text-local-patterns patterns)))
      (add-hook (intern (format "%s-hook" mode)) fn-name))))

;; FIXME obsolete :rotate
;;;###autoload
(def-setting! :rotate (modes &rest plist)
  :obsolete set-rotate-patterns!
  `(set-rotate-patterns! ,modes ,@plist))
