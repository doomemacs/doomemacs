;;; completion/ivy/autoload/posframe.el -*- lexical-binding: t; -*-
;;;###if (modulep! +childframe)

;;;###autoload
(defun +ivy-display-at-frame-center-near-bottom-fn (str)
  "TODO"
  (ivy-posframe--display str #'+ivy-poshandler-frame-center-near-bottom-fn))

;;;###autoload
(defun +ivy-poshandler-frame-center-near-bottom-fn (info)
  "TODO"
  (let ((parent-frame (plist-get info :parent-frame))
        (pos (posframe-poshandler-frame-center info)))
    (cons (car pos)
          (truncate (/ (frame-pixel-height parent-frame) 2)))))

