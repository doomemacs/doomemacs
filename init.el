;;; Emacs for the jaded vimmer
;;
;; Author: Henrik Lissner <henrik@lissner>
;; URL: https://github.com/hlissner/emacs.d
;;
;; These settings set up a very vim-like experience, with some of emacs goodness
;; squeezed into the cracks.
;;
;;; Code:

(cd "~") ; Default directory, instead of /
;; (setq debug-on-error t)

(server-mode t)
(unless (server-running-p) (server-start))

;; Global vars
(defvar my-dir (file-name-directory load-file-name))
(defvar my-core-dir (expand-file-name "init" my-dir))
(defvar my-modules-dir (expand-file-name "modules" my-dir))
(defvar my-themes-dir (expand-file-name "themes" my-dir))
(defvar my-elisp-dir (expand-file-name "elisp" my-dir))
(defvar my-tmp-dir (expand-file-name "tmp" my-dir))

;; Setup loadpaths
(add-to-list 'load-path my-core-dir)
(add-to-list 'load-path my-modules-dir)
(add-to-list 'load-path my-elisp-dir)
(add-to-list 'custom-theme-load-path my-themes-dir)

;; Font & color scheme
(load-theme 'brin t)
(defvar my-font "Inconsolata-14")

;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap
;;;;;;;;;;;;;;;;;;;;;;;

(dolist (module '(
	;; Just the... bear necessities...
	core				; Emacs core settings
	core-packages		; Package init & management
	core-ui				; Look and behavior of the emacs UI
	core-editor			; Text/code editor settings and behavior
	core-osx			; OSX-specific settings & functions
	core-project		; Project navigation settings & packages

	;; Modules to improve on emacs' heresy
	mod-ac				; Auto-complete engine & settings
	mod-snippets		; Snippet engine
	mod-git				; GIT tools/settings
	mod-fly				; Syntax and spell checkers
    mod-emmet           ; Zen-coding for HTML+CSS
	; mod-webdev		; Webdev tools (sass, js, etc)
	; mod-gamedev		; Gamedev tools (C++, love2D, html5)
	; mod-shell			; Terminal emulator settings

	;; Must be last
	core-keymaps		; Global & local keybindings for all modes
	))
  (require module))


;;;; Modes ;;;;;;;;;;;;;;;;;;;;;;;;

;; Associates a mode with a path regex. If the third parameter is t,
;; then don't try to install the mode (use for modes that are included
;; with emacs).
(associate-mode 'ruby-mode			'("\\.rb\\'" "\\.rake\\'" "Rakefile\\'"))
(associate-mode 'markdown-mode		'("\\.md\\'" "\\.markdown\\'" "/README"))
(associate-mode 'scss-mode	  		'("\\.scss\\'"))
(associate-mode 'org-mode	   		'("\\.org\\'" "\\.gtd\\'") t)
(associate-mode 'js-mode		    '("\\.js\\'") t)
(associate-mode 'json-mode			'("\\.json\\'" "\\.jshintrc\\'"))
(associate-mode 'web-mode	   		'("\\.\\(p\\)?htm\\(l\\)?\\'" "\\.tpl\\(\\.php\\)?\\'" "\\.erb\\'"))
(associate-mode 'lua-mode	   		'("\\.lua\\'"))
(associate-mode 'yaml-mode	  		'("\\.yml\\'"))
(associate-mode 'python-mode		'("\\.py\\'"))
(associate-mode 'c++-mode	   		'("\\.h\\'") t)
(associate-mode 'shell-script-mode  '("\\.zsh\\(rc\\|env\\)?\\'") t)

;; Specific mode-configs: DON'T NEED TO REQUIRE THESE.
;; They're here for your gf-ing convenience.
; modules/env-js-mode.el
; modules/env-ruby-mode.el
; modules/env-python-mode.el
; modules/env-lua-mode.el
; modules/env-python-mode.el

;;
