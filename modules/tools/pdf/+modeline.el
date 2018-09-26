;;; tools/pdf/+modeline.el -*- lexical-binding: t; -*-

(def-modeline-segment! +pdf-pages
  "Current and total page indicator for PDF documents."
  (format "P %d/%d" (pdf-view-current-page) (pdf-cache-number-of-pages)))

(if (featurep! :ui modeline)
    (def-modeline-format! '+pdf
      '(+mode-line-bar " " +mode-line-buffer-id "  " +pdf-pages)
      '(+mode-line-major-mode +mode-line-vcs))
  (def-modeline! '+pdf
    '(bar matches " " buffer-info "  " +pdf-pages)
    '(major-mode vcs)))

(defun +pdf|init-modeline ()
  (funcall (if (featurep! :ui modeline)
               #'set-modeline!
             #'doom-set-modeline)
           '+pdf))
(add-hook 'pdf-tools-enabled-hook #'+pdf|init-modeline)
