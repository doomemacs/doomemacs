;;; emacs/electric/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-electric! (modes &rest plist)
  "Declare :words (list of strings) or :chars (lists of chars) in MODES that
trigger electric indentation."
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+electric|init-%s" mode))))
      (cond ((null (car-safe plist))
             (remove-hook hook fn)
             (unintern fn nil))
            ((fset fn
                   (lambda ()
                     (cl-destructuring-bind (&key chars words) plist
                       (electric-indent-local-mode +1)
                       (if chars (setq electric-indent-chars chars))
                       (if words (setq +electric-indent-words words)))))
             (add-hook hook fn))))))

;; FIXME obsolete :electric
;;;###autoload
(def-setting! :electric (modes &rest plist)
  :obsolete set-electric!
  `(set-electric! ,modes ,@plist))
