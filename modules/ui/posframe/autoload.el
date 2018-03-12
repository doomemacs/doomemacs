;;; ui/posframe/autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +posframe-ivy-display-at-frame-center-near-bottom (str)
  "TODO"
  (ivy-posframe--display str #'+posframe-poshandler-frame-center-near-bottom))

;;;###autoload
(defun +posframe-poshandler-frame-center-near-bottom (info)
  "TODO"
  (let ((pos (posframe-poshandler-frame-center info)))
    (cons (car pos)
          (truncate (* (cdr pos) 1.60)))))
