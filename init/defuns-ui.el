(eval-when-compile (require 'cl))

;;;###autoload
(defun toggle-transparency ()
  (interactive)
  (let* ((alpha (frame-parameter nil 'alpha))
         (alpha-val (if (listp alpha) (car alpha) alpha)))
    (if (/= alpha-val 97)
        (set-frame-parameter nil 'alpha 97)
      (set-frame-parameter nil 'alpha 0))))

;;;###autoload
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

