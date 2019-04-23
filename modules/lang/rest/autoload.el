;;; lang/rest/autoload.el -*- lexical-binding: t; -*-

(defun +rest-request-at-point-p (&optional pos)
  (save-excursion
    (if pos (goto-char pos))
    (beginning-of-line)
    (and (re-search-forward restclient-method-url-regexp
                            (line-end-position) t)
         (not (nth 4 (syntax-ppss))))))

;;;###autoload
(defun +rest/dwim-at-point ()
  "TODO"
  (interactive)
  (when (+rest-request-at-point-p)
    (restclient-http-send-current-stay-in-window)))

;;;###autoload
(defun +rest/fold-all ()
  "TODO"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((last (point)))
      (while (and (restclient-jump-next)
                  (not (= last (setq last (point)))))
        (unless (overlays-at (line-end-position))
          (restclient-toggle-body-visibility))))))
