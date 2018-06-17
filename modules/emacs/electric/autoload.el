;;; emacs/electric/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-electric! (modes &rest plist)
  "Declare :words (list of strings) or :chars (lists of chars) in MODES that
trigger electric indentation."
  (declare (indent 1))
  (unless plist
    (signal 'wrong-number-of-arguments
            (list '(:char :words) plist)))
  (cl-destructuring-bind (&key chars words) plist
    (dolist (mode (doom-enlist modes))
      (let ((fn (intern (format "+electric|init-%s" mode))))
        (fset fn
              (lambda ()
                (electric-indent-local-mode +1)
                (if chars (setq electric-indent-chars chars))
                (if words (setq +electric-indent-words words))))
        (add-hook (intern (format "%s-hook" mode)) fn)))))

;; FIXME obsolete :electric
;;;###autoload
(def-setting! :electric (modes &rest plist)
  :obsolete set-electric!
  `(set-electric! ,modes ,@plist))
