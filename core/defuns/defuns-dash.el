;;; defuns-dash.el --- Dash.app integration

;;;###autoload
(defun doom/dash-at-pt ()
  (interactive)
  (if (evil-visual-state-p)
      (dash-at-point-run-search (buffer-substring-no-properties (region-beginning) (region-end))
                                (dash-at-point-guess-docset))
    (dash-at-point)))

;;;###autoload (autoload 'doom:dash "defuns-dash" nil t)
(evil-define-command doom:dash (bang input)
  (interactive "<!><a>")
  (let ((docset (unless bang (dash-at-point-guess-docset)))
        (query input))
    (when (evil-visual-state-p)
      (setq query (concat (buffer-substring-no-properties (region-beginning) (region-end))
                          " " query)))
    (when (or (not query) (zerop (length query)))
      (setq query (thing-at-point 'symbol)))
    (dash-at-point-run-search query docset)))

(provide 'defuns-dash)
;;; defuns-dash.el ends here
