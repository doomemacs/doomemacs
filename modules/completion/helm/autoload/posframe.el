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
;;;###autoload
(defun +helm-posframe-display (buffer &optional _resume)
  "TODO"
  (setq helm--buffer-in-new-frame-p t)
  (let ((solaire-p (bound-and-true-p solaire-mode))
        (params (copy-sequence +helm-posframe-parameters)))
    (let-alist params
      (require 'posframe)
      (posframe-show
       (setq +helm--posframe-buffer buffer)
       :position (point)
       :poshandler +helm-posframe-handler
       :width
       (max (cl-typecase .width
              (integer .width)
              (float (truncate (* (frame-width) .width)))
              (function (funcall .width))
              (t 0))
            .min-width)
       :height
       (max (cl-typecase .height
              (integer .height)
              (float (truncate (* (frame-height) .height)))
              (function (funcall .height))
              (t 0))
            .min-height)
       :override-parameters
       (dolist (p '(width height min-width min-height) params)
         (setq params (delq (assq p params) params)))))
    ;;
    (unless (or (null +helm-posframe-text-scale)
                (= +helm-posframe-text-scale 0))
      (with-current-buffer buffer
        (when (and (featurep 'solaire-mode)
                   (not solaire-p))
          (solaire-mode +1))
        (text-scale-set +helm-posframe-text-scale)))))

;;;###autoload
(defun +helm|posframe-cleanup ()
  "TODO"
  ;; Ensure focus is properly returned to the underlying window, by forcing a
  ;; chance in buffer/window focus. This gives the modeline a chance to refresh.
  (switch-to-buffer +helm--posframe-buffer t)
  ;;
  (posframe-delete +helm--posframe-buffer))


;;;###autoload
(defun +helm*fix-get-font-height (orig-fn position)
  (ignore-errors (funcall orig-fn position)))
