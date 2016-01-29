;;; defuns-python.el

;;;###autoload
(defun narf*anaconda-mode-doc-buffer ()
  "Delete the window on escape or C-g."
  (with-current-buffer (get-buffer "*anaconda-doc*")
    (local-set-key [escape] 'anaconda-nav-quit)
    (local-set-key [?\C-g] 'anaconda-nav-quit)))

(provide 'defuns-python)
;;; defuns-python.el ends here
