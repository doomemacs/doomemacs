;;; completion/helm/autoload/posframe.el -*- lexical-binding: t; -*-

(add-hook 'helm-cleanup-hook #'+helm|posframe-cleanup)

;;;###autoload
(defun +helm-poshandler-frame-center-near-bottom (info)
  "Display the child frame in the center of the frame, slightly closer to the
bottom, which is easier on the eyes on big displays."
  (let ((parent-frame (plist-get info :parent-frame))
        (pos (posframe-poshandler-frame-center info)))
    (cons (car pos)
          (truncate (/ (frame-pixel-height parent-frame)
                       2)))))

(defvar +helm--posframe-buffer nil)
(defvar +helm--posframe-last-window nil)
;;;###autoload
(defun +helm-posframe-display (buffer &optional _resume)
  "TODO"
  (setq +helm--posframe-last-window (selected-window))
  (require 'posframe)
  (setq helm--buffer-in-new-frame-p t)
  (posframe-show
   (setq +helm--posframe-buffer buffer)
   :poshandler +helm-posframe-handler
   :internal-border-width +helm-posframe-border-width
   :respect-header-line t
   :respect-mode-line t
   :width
   (cond ((functionp helm-display-buffer-default-width)
          (funcall helm-display-buffer-default-width))
         ((integerp helm-display-buffer-default-width)
          helm-display-buffer-default-width)
         ((floatp helm-display-buffer-default-width)
          (truncate (* (frame-width) helm-display-buffer-default-width)))
         ((min (max (truncate (* (frame-width) 0.8))
                    100)
               140)))
   :height
   (cond ((functionp helm-display-buffer-default-height)
          (funcall helm-display-buffer-default-height))
         ((integerp helm-display-buffer-default-height)
          helm-display-buffer-default-height)
         ((floatp helm-display-buffer-default-height)
          (truncate (* (frame-height) helm-display-buffer-default-height)))
         ((truncate (* (frame-height) 0.4)))))
  (when +helm-posframe-font-scale
    (with-current-buffer buffer
      (text-scale-set +helm-posframe-font-scale))))

;;;###autoload
(defun +helm|posframe-cleanup ()
  "TODO"
  ;; Ensure the underlying window is switched to, to ensure that frame is given
  ;; proper focus; this gives the modeline a chance to refresh.
  (select-window +helm--posframe-last-window)
  (setq +helm--posframe-last-frame nil)
  ;;
  (posframe-delete +helm--posframe-buffer))

