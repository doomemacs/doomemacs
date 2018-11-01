;;; ui/frame-workflow/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +frame-workflow/ivy-switch-frame ()
  "Pops up an ivy buffer and switch to selected frame."
  (interactive)
  (ivy-read
   "Switch to frame: "
   (mapcar
    (lambda (observer)
      (let* ((frame (oref observer frame))
             (subject (frame-workflow--subject-name observer))
             (title (frame-parameter frame 'name)))
        `(,subject . ,frame)))
    frame-workflow--observer-list)
   :action (lambda (alist)
             (frame-workflow--select-frame (cdr alist)))))
