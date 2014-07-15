;;;; core.el - Emacs for the jaded Vimmer
;;
;; Author: Henrik Lissner <henrik@lissner>
;; URL: https://github.com/hlissner/emacs.d
;;
;; These settings set up a very vim-like experience, with some of emacs goodness
;; squeezed in between the cracks.

(cd "~")                    ; Default directory, instead of /
(setq load-prefer-newer t)  ; Always load newest byte code

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
(defvar my-font "Ubuntu Mono-15")

;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap
;;;;;;;;;;;;;;;;;;;;;;;

(dolist (module '(
      core               ; Emacs core settings
      core-packages      ; package init & management
      core-ui            ; Look and behavior of the emacs UI
      core-editor        ; Text/code editor settings and behavior
      core-project       ; Project navigation settings & packages
      core-osx           ; OSX-specific settings & functions
      core-keymaps       ; Global & local keybindings for all modes

      ;; Editor essentials
      ; mod-snippets     ; Snippet templates
      ; mod-git          ; GIT tools/settings
      ; mod-writer       ; Setup/settings for non-code writing
      ; mod-shell        ; Goodies for running shell in emacs
      ; mod-webdev       ; Webdev tools (sass, js, etc)
      ; mod-gamedev      ; Gamedev tools (C++, love2D, html5)
      ))
  (require module))


;;;; Modes ;;;;;;;;;;;;;;;;;;;;;;;;

; (associate-mode '(".rb" "RakeFile") 'ruby-mode)
; (associate-mode '(".md" ".markdown") 'markdown-mode)
; (associate-mode ".lua" 'lua-mode)
; (associate-mode ".scss" 'scss-mode)
; (associate-mode ".yml" 'yaml-mode)
; (associate-mode '(".js" ".json") 'js2-mode)
; (associate-mode ".py" 'python-mode)
