;;; lang/lua/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +lua/open-repl ()
  "Open Lua REPL."
  (interactive)
  (lua-start-process "lua" "lua")
  (pop-to-buffer lua-process-buffer))

;;;###autoload
(defun +lua/run-love-game ()
  "Run the current project with Love2D."
  (interactive)
  (when-let (root (+lua-love-project-root))
    (async-shell-command
     (format "%s %s"
             (or (executable-find "love")
                 (if IS-MAC "open -a love.app"))
             (shell-quote-argument (file-name-directory root))))))

;;;###autoload
(defun +lua-love-project-root ()
  "Returns the directory where a main.lua exists.

Returns nil if 'love' executable can't be found."
  (and (executable-find "love")
       (locate-dominating-file buffer-file-name "main.lua")))
