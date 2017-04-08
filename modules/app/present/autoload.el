;;; app/present/autoload.el

;;;###autoload
(defun +present/buffer ()
  (interactive)
  (require 'simple-httpd)
  (unless (process-status "httpd")
    (httpd-start))
  (unless impatient-mode
    (impatient-mode +1)))

;;;###autoload
(define-minor-mode +present/big-mode
  :init-value nil
  :lighter " BIG"
  :global t
  (if +present-big-mode
      (set-frame-font +present-big-font t t)
    (set-frame-font +present-original-font t t)))

;;;###autoload
(defun +present/stream ()
  "Resize the frame pixelwise, so that it fits directly into my livecoding.tv
streaming layout."
  (interactive)
  (set-frame-width (selected-frame) 1325 nil t)
  (set-frame-height (selected-frame) 1080 nil t))
