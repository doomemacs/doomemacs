;;; emacs/electric/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-electric! (modes &rest plist)
  "Declare :words (list of strings) or :chars (lists of chars) in MODES that
trigger electric indentation."
  (declare (indent 1))
  (cl-destructuring-bind (&key char words) plist
    (when (or chars words)
      (let* ((name (mapconcat #'symbol-name modes "-"))
             (fn (intern (format "+electric-indent--init-%s" name))))
        (fset fn
              (lambda () (electric-indent-local-mode +1)
                (if chars (setq electric-indent-chars chars))
                (if words (setq +electric-indent-words words))))
        (dolist (mode modes)
          (add-hook (intern (format "%s-hook" mode)) fn-name))))))

;; FIXME obsolete :electric
;;;###autoload
(def-setting! :electric (modes &rest plist)
  "Declare :words (list of strings) or :chars (lists of chars) in MODES that
trigger electric indentation."
  :obsolete set-electric!
  `(set-electric! ,modes ,@plist))
