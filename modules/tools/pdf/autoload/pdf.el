;;; tools/pdf/autoload/pdf.el -*- lexical-binding: t; -*-
;;;###autoload
(defun *pdf-pdf-view-use-scaling-p ()
    "Return t if scaling should be used."
    (and (or (and (eq system-type 'darwin) (string-equal emacs-version "27.0.50"))
             (memq (pdf-view-image-type)
                   '(imagemagick image-io)))
         pdf-view-use-scaling))
;;;###autoload
(defun *pdf-pdf-view-create-page (page &optional window)
  "Create an image of PAGE for display on WINDOW."
  (let* ((size (pdf-view-desired-image-size page window))
         (width (if (not (pdf-view-use-scaling-p))
                    (car size)
                  (* 2 (car size))))
         (data (pdf-cache-renderpage
                page width width))
         (hotspots (pdf-view-apply-hotspot-functions
                    window page size)))
    (pdf-view-create-image data
      :width width
      :scale (if (pdf-view-use-scaling-p) 0.5 1)
      :map hotspots
      :pointer 'arrow)))
