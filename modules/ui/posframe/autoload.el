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
          (truncate (* (cdr pos) 1.4)))))

;;;###autoload
(defun +posframe|delete-on-escape ()
  "TODO"
  (when (cl-loop for frame in (frame-list)
                 if (and (frame-parameter frame 'posframe-buffer)
                         (not (frame-visible-p frame)))
                 return t)
    (posframe-delete-all)))
