;;; Emacs for the jaded vimmer
;;
;; Author: Henrik Lissner <henrik@lissner>
;; URL: https://github.com/hlissner/emacs.d
;;
;; These settings set up a very vim-like experience, with some of emacs goodness
;; squeezed into the cracks.
;;
;;; Code:

(cd "~")                 ; Default directory, instead of /
(setq skip-installs t)   ; Don't check packages (for slightly speedier startup)
;; (setq use-package-verbose t)

(server-mode t)
(unless (server-running-p) (server-start))
(desktop-save-mode t)

;; Global vars
(defvar my/dir (file-name-directory load-file-name))
(defvar my/init-dir (expand-file-name "init" my/dir))
(defvar my/themes-dir (expand-file-name "themes" my/dir))
(defvar my/elisp-dir (expand-file-name "elisp" my/dir))
(defvar my/tmp-dir (expand-file-name "tmp" my/dir))
(defvar my/snippets-dir (expand-file-name "snippets" my/dir))

(defvar my/theme 'brin)
(defvar my/font "Inconsolata-14")

(add-to-list 'load-path my/init-dir)

(mapc 'require
  '(core                ; Just the... bear necessities...

	;; Modules to improve on emacs' heresy
    init-project        ; Project navigation tools & settings
    init-ac             ; Auto-complete engine & settings
    init-snippets       ; Snippet engine
    init-git            ; GIT tools/settings
    init-fly            ; Syntax and spell checker

    mod-text            ; Plain text editing (markdown, text)
    mod-org             ; Org-mode: personal gtd/notes
    mod-dev             ; Generic environment for all programming
    mod-webdev          ; Environment for webdev (SCSS, PHP, Rails, Jekyll)
    mod-gamedev         ; Environment for gamedev (C++, Lua, HTML5, etc)
    mod-eclim           ; Integration into eclipse (for Java)
	))
