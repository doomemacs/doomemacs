;;; defuns-lua.el

;;;###autoload
(defun doom/inf-lua ()
  (interactive)
  (lua-start-process "lua" "lua")
  (pop-to-buffer lua-process-buffer))

(provide 'defuns-lua)
;;; defuns-lua.el ends here
