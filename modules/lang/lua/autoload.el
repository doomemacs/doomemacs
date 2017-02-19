;;; lang/lua/autoload.el

;;;###autoload
(defun +lua/repl ()
  (interactive)
  (lua-start-process "lua" "lua")
  (pop-to-buffer lua-process-buffer))


