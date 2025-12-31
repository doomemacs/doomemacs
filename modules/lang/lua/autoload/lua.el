;;; lang/lua/autoload/lua.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +lua-lsp-dir (file-name-concat doom-profile-data-dir "lsp" "lua-language-server/")
  "Absolute path to the directory of sumneko's lua-language-server.

This directory MUST contain the 'main.lua' file and be the in-source build of
lua-language-server.")

;;;###autoload
(defun +lua-generate-lsp-server-command ()
  ;; The absolute path to lua-language-server binary is necessary because the
  ;; bundled dependencies aren't found otherwise. The only reason this is a
  ;; function is to dynamically change when/if `+lua-lsp-dir' does
  (list (or (executable-find "lua-language-server")
            (doom-path +lua-lsp-dir
                       (cond ((featurep :system 'macos)   "bin/macOS")
                             ((featurep :system 'linux)   "bin/Linux")
                             ((featurep :system 'windows) "bin/Windows"))
                       "lua-language-server"))
        "-E" "-e" "LANG=en"
        (doom-path +lua-lsp-dir "main.lua")))

(defun +lua-love-build-command ()
  (when-let (root (+lua-love-project-root))
    (format "%s %s"
            (if (executable-find "love")
                "love"
              (if (featurep :system 'macos) "open -a love.app"))
            (shell-quote-argument root))))

;;;###autoload
(defun +lua-love-project-root (&optional dir)
  "Return directory (up from DIR) where a main.lua or main.moon exists.

Returns nil if 'love' executable can't be found."
  (when (executable-find "love")
    (when-let ((dir (or dir (doom-project-root))))
      (if (doom-project-p dir)
          (file-name-directory
           (or (file-exists-p! (or "main.lua" "src/main.lua") dir)
               (and (modulep! +moonscript)
                    (file-exists-p! (or "main.moon" "src/main.moon") dir))
               ""))
        ;; Since Love2D games are likely to be prototypes, they may not be in a
        ;; well-formed project as far as projecitle is concerned, so we search for
        ;; main.lua/main.moon up the file tree as a backup.
        (or (projectile-locate-dominating-file dir "main.lua")
            (when-let (root (projectile-locate-dominating-file dir "src/main.lua"))
              (expand-file-name "src" root))
            (and (modulep! +moonscript)
                 (or (projectile-locate-dominating-file dir "main.moon")
                     (when-let (root (projectile-locate-dominating-file dir "src/main.moon"))
                       (expand-file-name "src" root)))))))))


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
  (if-let* ((cmd (+lua-love-build-command)))
      (async-shell-command cmd)
    (user-error "Couldn't find love project")))
