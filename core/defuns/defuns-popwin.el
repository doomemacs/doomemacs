;;;###autoload
(defun narf/popwin-toggle ()
  (interactive)
  (if (popwin:popup-window-live-p)
      (popwin:close-popup-window)
    (popwin:popup-last-buffer)))


(provide 'defuns-popwin)
;;; defuns-popwin.el ends here
