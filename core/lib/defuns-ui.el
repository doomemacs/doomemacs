;;; defuns-ui.el
;; for ../core-ui.el

;;;###autoload
(defun narf:toggle-transparency ()
  (interactive)
  (let* ((alpha (frame-parameter nil 'alpha))
         (alpha-val (if (listp alpha) (car alpha) alpha)))
    (if (/= alpha-val 97)
        (set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 0))))

;;;###autoload
(defun narf:toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defvar narf--big-mode nil)
;;;###autoload
(defun narf:toggle-big-mode ()
  (interactive)
  (if narf--big-mode
      (set-frame-font (apply #'font-spec narf-default-font))
    (set-frame-font (apply #'font-spec narf-big-font)))
  (setq narf--big-mode (not narf--big-mode)))

(provide 'defuns-ui)
;;; defuns-ui.el ends here
