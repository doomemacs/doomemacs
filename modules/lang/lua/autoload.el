;;; lang/lua/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +lua/repl ()
  "Open Lua REPL."
  (interactive)
  (lua-start-process "lua" "lua")
  (pop-to-buffer lua-process-buffer))

