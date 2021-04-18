;;; lang/lua/autoload/lua.el -*- lexical-binding: t; -*-

(defun +lua-love-build-command ()
  (when-let (root (+lua-love-project-root))
    (format "%s %s"
            (if (executable-find "love")
                "love"
              (if IS-MAC "open -a love.app"))
            (shell-quote-argument root))))

;;;###autoload
(defun +lua-love-project-root ()
  "Returns the directory where a main.lua or main.moon exists.

Returns nil if 'love' executable can't be found."
  (when (executable-find "love")
    (if (doom-project-p)
        (file-name-directory
         (or (project-file-exists-p! (or "main.lua" "src/main.lua"))
             (and (featurep! +moonscript)
                  (project-file-exists-p! (or "main.moon" "src/main.moon")))
             ""))
      ;; Since Love2D games are likely to be prototypes, they may not be in a
      ;; well-formed project as far as projecitle is concerned, so we search for
      ;; main.lua/main.moon up the file tree as a backup.
      (or (projectile-locate-dominating-file default-directory "main.lua")
          (when-let (root (projectile-locate-dominating-file default-directory "src/main.lua"))
            (expand-file-name "src" root))
          (and (featurep! +moonscript)
               (or (projectile-locate-dominating-file default-directory "main.moon")
                   (when-let (root (projectile-locate-dominating-file default-directory "src/main.moon"))
                     (expand-file-name "src" root))))))))


;;
;;; Commands

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
