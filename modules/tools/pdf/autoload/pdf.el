;;; tools/pdf/autoload/pdf.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +pdf--supply-width-to-create-image-calls-a (orig-fn &rest args)
  (cl-letf* ((old-create-image (symbol-function #'create-image))
             ((symbol-function #'create-image)
              (lambda (file-or-data &optional type data-p &rest props)
                (apply old-create-image file-or-data type data-p
                       :width (car (pdf-view-image-size))
                       props))))
    (apply orig-fn args)))

;;;###autoload
(defun +pdf--util-frame-scale-factor-a (orig-fn)
  (if (and pdf-view-use-scaling
           (memq (pdf-view-image-type) '(imagemagick image-io))
           (fboundp 'frame-monitor-attributes))
      (funcall orig-fn)
    ;; Add special support for retina displays on MacOS
    (if (and (eq (framep-on-display) 'ns)
             EMACS27+)
        2
      1)))

;;;###autoload
(defun +pdf--view-use-scaling-p-a ()
  "Returns t if on ns window-system on Emacs 27+."
  (and (eq (framep-on-display) 'ns)
       EMACS27+
       pdf-view-use-scaling))
