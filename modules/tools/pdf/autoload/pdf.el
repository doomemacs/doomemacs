;;; tools/pdf/autoload/pdf.el -*- lexical-binding: t; -*-
;;;###autoload
(defun *pdf-pdf-view-use-scaling-p ()
  "Return t if scaling should be used."
  (and (or (and (eq (framep-on-display) 'ns) (string-equal emacs-version "27.0.50"))
           (memq (pdf-view-image-type)
                 '(imagemagick image-io)))
       pdf-view-use-scaling))
;;;###autoload
(defun *pdf-pdf-annot-show-annotation (a &optional highlight-p window)
  "Make annotation A visible.

Turn to A's page in WINDOW, and scroll it if necessary.

If HIGHLIGHT-P is non-nil, visually distinguish annotation A from
other annotations."

  (save-selected-window
    (when window (select-window window))
    (pdf-util-assert-pdf-window)
    (let* ((page (pdf-annot-get a 'page))
           (size (pdf-view-image-size))
           (width (car size)))
      (unless (= page (pdf-view-current-page))
        (pdf-view-goto-page page))
      (let ((edges (pdf-annot-get-display-edges a)))
        (when highlight-p
          (pdf-view-display-image
           (pdf-view-create-image
               (pdf-cache-renderpage-highlight
                page width
                `("white" "steel blue" 0.35 ,@edges))
             :map (pdf-view-apply-hotspot-functions
                   window page size)
             :width width)))
        (pdf-util-scroll-to-edges
         (pdf-util-scale-relative-to-pixel (car edges)))))))
;;;###autoload
(defun *pdf-pdf-isearch-hl-matches (current matches &optional occur-hack-p)
  "Highlighting edges CURRENT and MATCHES."
  (cl-check-type current pdf-isearch-match)
  (cl-check-type matches (list-of pdf-isearch-match))
  (cl-destructuring-bind (fg1 bg1 fg2 bg2)
      (pdf-isearch-current-colors)
    (let* ((width (car (pdf-view-image-size)))
           (page (pdf-view-current-page))
           (window (selected-window))
           (buffer (current-buffer))
           (tick (cl-incf pdf-isearch--hl-matches-tick))
           (pdf-info-asynchronous
            (lambda (status data)
              (when (and (null status)
                         (eq tick pdf-isearch--hl-matches-tick)
                         (buffer-live-p buffer)
                         (window-live-p window)
                         (eq (window-buffer window)
                             buffer))
                (with-selected-window window
                  (when (and (derived-mode-p 'pdf-view-mode)
                             (or isearch-mode
                                 occur-hack-p)
                             (eq page (pdf-view-current-page)))
                    (pdf-view-display-image
                     (pdf-view-create-image data
                       :width width))))))))
      (pdf-info-renderpage-text-regions
       page width t nil
       `(,fg1 ,bg1 ,@(pdf-util-scale-pixel-to-relative
                      current))
       `(,fg2 ,bg2 ,@(pdf-util-scale-pixel-to-relative
                      (apply 'append
                             (remove current matches))))))))
;;;###autoload
(defun *pdf-pdf-util-frame-scale-factor ()
  "Return the frame scale factor depending on the image type used for display.
When `pdf-view-use-scaling' is non-nil and imagemagick or
image-io are used as the image type for display, return the
backing-scale-factor of the frame if available. If a
backing-scale-factor attribute isn't available, return 2 if the
frame's PPI is larger than 180. Otherwise, return 1."
  (if (and pdf-view-use-scaling
           (memq (pdf-view-image-type) '(imagemagick image-io))
           (fboundp 'frame-monitor-attributes))
      (or (cdr (assq 'backing-scale-factor (frame-monitor-attributes)))
          (if (>= (pdf-util-frame-ppi) 180)
              2
            1))
    (if (and (eq (framep-on-display) 'ns) (string-equal emacs-version "27.0.50"))
        2
      1)))
;;;###autoload
(defun *pdf-pdf-view-display-region (&optional region rectangle-p)
  ;; TODO: write documentation!
  (unless region
    (pdf-view-assert-active-region)
    (setq region pdf-view-active-region))
  (let ((colors (pdf-util-face-colors
                 (if rectangle-p 'pdf-view-rectangle 'pdf-view-region)
                 (bound-and-true-p pdf-view-dark-minor-mode)))
        (page (pdf-view-current-page))
        (width (car (pdf-view-image-size))))
    (pdf-view-display-image
     (pdf-view-create-image
         (if rectangle-p
             (pdf-info-renderpage-highlight
              page width nil
              `(,(car colors) ,(cdr colors) 0.35 ,@region))
           (pdf-info-renderpage-text-regions
            page width nil nil
            `(,(car colors) ,(cdr colors) ,@region)))
       :width width))))
