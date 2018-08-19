;;; completion/ivy/autoload/posframe.el -*- lexical-binding: t; -*-
;;;###if (featurep! +childframe)

;;;###autoload
(defun +ivy-display-at-frame-center-near-bottom (str)
  "TODO"
  (ivy-posframe--display str #'+ivy-poshandler-frame-center-near-bottom))

;;;###autoload
(defun +ivy-poshandler-frame-center-near-bottom (info)
  "TODO"
  (let ((parent-frame (plist-get info :parent-frame))
        (pos (posframe-poshandler-frame-center info)))
    (cons (car pos)
          (truncate (/ (frame-pixel-height parent-frame) 2)))))

