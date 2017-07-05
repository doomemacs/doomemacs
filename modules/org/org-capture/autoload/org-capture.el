;;; org/org-capture/autoload/org-capture.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-capture/dwim (&optional string key)
  "Sends STRING, the current selection or prompted input to `org-capture'.

Uses the capture template specified by KEY. Otherwise, prompts you for one."
  (interactive)
  (let ((key (or key "n")))
    (if-let (string (cond ((not (equal string ""))
                           string)
                          ((region-active-p)
                           (buffer-substring-no-properties
                            (region-beginning)
                            (region-end)))))
        (org-capture-string string key)
      (org-capture nil key))))
