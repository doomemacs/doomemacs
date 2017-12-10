;;; lang/org/autoload/org-capture.el -*- lexical-binding: t; -*-
;;;###if (featurep! +capture)

(when (featurep! :feature evil)
;;;###autoload (autoload '+org-capture:open "lang/org/autoload/org-capture" nil t)
  (evil-define-operator +org-capture:open (&optional beg end)
    "Evil ex interface to `+org-capture/dwim'."
    :move-point nil :type inclusive
    (interactive "<r>")
    (+org-capture/open
     (unless (or (evil-normal-state-p) (evil-insert-state-p))
       (buffer-substring beg end)))))

;;;###autoload
(defun +org-capture/open (&optional string key)
  "Sends STRING, the current selection or prompted input to `org-capture'.

Uses the capture template specified by KEY. Otherwise, prompts you for one."
  (interactive)
  (let ((key (or key "n")))
    (if-let* ((string (cond ((not (equal string ""))
                             string)
                            ((region-active-p)
                             (buffer-substring-no-properties
                              (region-beginning)
                              (region-end))))))
        (org-capture-string string key)
      (org-capture nil key))))


;; --- External frame ---------------------

(defvar +org-capture-window-params
  `((name . "org-capture")
    (width . 70)
    (height . 25)
    (window-system . ,(cond (IS-MAC 'ns)
                            (IS-LINUX 'x)
                            (t 'w32)))
    ,(if IS-LINUX '(display . ":0")))
  "TODO")

;;;###autoload
(defun +org-capture|cleanup-frame ()
  "Closes the org-capture frame once done adding an entry."
  (when (+org-capture-frame-p)
    (delete-frame nil t)))

;;;###autoload
(defun +org-capture-frame-p (&rest _)
  "Return t if the current frame is an org-capture frame opened by
`+org-capture/open-frame'."
  (equal "org-capture" (frame-parameter nil 'name)))

;;;###autoload
(defun +org-capture/open-frame (&optional string key)
  "Opens the org-capture window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (require 'org)
  (let (after-make-frame-functions before-make-frame-hook)
    (let ((frame (if (+org-capture-frame-p)
                     (selected-frame)
                   (make-frame +org-capture-window-params))))
      (with-selected-frame frame
        (condition-case ex
            (cl-letf (((symbol-function #'pop-to-buffer)
                       (symbol-function #'switch-to-buffer)))
              (if (and (stringp string)
                       (not (string-empty-p string)))
                  (org-capture-string string key)
                (org-capture nil key))
              (when (featurep 'solaire-mode)
                (solaire-mode +1)))
          ('error
           (message "org-capture: %s" (error-message-string ex))
           (delete-frame frame)))))))

;;;###autoload
(defun +org-capture-available-keys ()
  "TODO"
  (string-join (mapcar #'car org-capture-templates) ""))
