;;; ui/posframe/autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +posframe-ivy-display-at-frame-center-near-bottom (str)
  "TODO"
  (ivy-posframe--display str #'+posframe-poshandler-frame-center-near-bottom))

;;;###autoload
(defun +posframe-poshandler-frame-center-near-bottom (info)
  "TODO"
  (let ((parent-frame (plist-get info :parent-frame))
        (pos (posframe-poshandler-frame-center info)))
    (cons (car pos)
          (truncate (/ (frame-pixel-height parent-frame) 1.6)))))

;;;###autoload
(defun +posframe|delete-on-escape ()
  "TODO"
  (unless (frame-parameter (selected-frame) 'posframe-buffer)
    (cl-loop for frame in (frame-list)
             if (and (frame-parameter frame 'posframe-buffer)
                     (not (frame-visible-p frame)))
             do (delete-frame frame))
    (dolist (buffer (buffer-list))
      (let ((frame (buffer-local-value 'posframe--frame buffer)))
        (when (and frame (or (not (frame-live-p frame))
                             (not (frame-visible-p frame))))
          (posframe--kill-buffer buffer))))))
