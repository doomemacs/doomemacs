;;; tools/ein/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ein/ace-link-ein ()
  "Ace jump to links in ein notebooklist."
  (interactive)
  (let ((res (avy-with +ein/ace-link-ein
               (avy--process
                (+ein--collect-ein-buffer-links)
                #'avy--overlay-pre))))
                ;(avy--style-fn avy-style)))))
    (when (numberp res)
      (goto-char (1+ res))
      (widget-button-press (point)))))

;;;###autoload
(defun +ein--collect-ein-buffer-links ()
  (interactive)
  (let ((end (window-end))
        points)
    (save-excursion
      (goto-char (window-start))
      (while (re-search-forward "~?/.+\\|\s\\[" end t)
        (push (+ (match-beginning 0) 1) points))
      (nreverse points))))

