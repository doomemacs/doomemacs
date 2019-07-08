;;; lang/lua/autoload/lua.el -*- lexical-binding: t; -*-

(defun +lua-love-build-command ()
  (when-let (root (+lua-love-project-root))
    (format "%s %s"
            (if (executable-find "love")
                "love"
              (if IS-MAC "open -a love.app"))
            (shell-quote-argument root))))

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
  (if-let (cmd (+lua-love-build-command))
      (async-shell-command cmd)
    (user-error "Couldn't find love project")))

;;;###autoload
(defun +lua-love-project-root ()
  "Returns the directory where a main.lua exists.

Returns nil if 'love' executable can't be found."
  (when (executable-find "love")
    (or (and (projectile-locate-dominating-file default-directory "main.lua")
             (when-let (root (projectile-locate-dominating-file default-directory "src/main.lua"))
               (expand-file-name "src" root)))
        (and (featurep! +moonscript)
             (projectile-locate-dominating-file default-directory "main.moon")
             (when-let (root (projectile-locate-dominating-file default-directory "src/main.moon"))
               (expand-file-name "src" root))))))
