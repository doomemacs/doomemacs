;;; defuns-lua.el

;;;###autoload
(defun narf/inf-lua ()
  (interactive)
  (lua-start-process "lua" "lua")
  (pop-to-buffer lua-process-buffer))

(provide 'defuns-lua)
;;; defuns-lua.el ends here
