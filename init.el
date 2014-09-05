;;; Emacs for the jaded vimmer
;;
;; Author: Henrik Lissner <henrik@lissner>
;; URL: https://github.com/hlissner/emacs.d
;;
;; These settings set up a vim-like experience, with some of emacs
;; goodness squeezed into the cracks.
;;
;;; Code:

(cd "~")                 ; Default directory, instead of /
;; (setq use-package-verbose t)

(require 'cask)
(cask-initialize)

(when window-system
  (server-mode t)
  (unless (server-running-p) (server-start)))

;; Global vars
(defconst *dir           (file-name-directory load-file-name))
(defconst *init-dir      (expand-file-name "init" *dir))
(defconst *themes-dir    (expand-file-name "themes" *dir))
(defconst *elisp-dir     (expand-file-name "elisp" *dir))
(defconst *snippets-dir  (expand-file-name "snippets" *dir))
(defconst *ac-dicts-dir  (expand-file-name "ac-dict" *dir))

(defconst *theme  'brin)
(defconst *font   "Inconsolata-16")
;; (defconst my/font   "Ubuntu-Mono-15")

(add-to-list 'load-path *init-dir)

(mapc 'require
  '(core                ; Just the... bear necessities...

    ;;; These are implicitly loaded from core.el, leave them commented!
    ;; core-editor      ; Internal config for global editor behavior
    ;; core-ui          ; User interface layout & behavior
    ;; core-osx         ; Mac-specific config
    ;; my-keymaps       ; My keybindings (loaded on after-init-hook)

    my-defuns           ; Personal functions

    ;; Modules to improve on emacs' heresy
    init-ido            ; Ido setup
    init-project        ; Project navigation tools & settings
    init-ac             ; Auto-complete engine & settings
    init-snippets       ; Snippet engine
    init-git            ; GIT tools/settings
    init-fly            ; Syntax and spell checker
    init-text           ; Plain text editing (markdown, text)
    init-org            ; Org-mode: personal gtd/notes
    init-dev            ; Generic environment for all programming
    init-ruby
    init-python
    init-webdev         ; Environment for webdev (SCSS, PHP, Rails, Jekyll)
    init-love           ; Love.app gamedev
    init-cpp            ; C++ gamedev
    init-eclim          ; Integration into eclipse (for Java)
    init-csharp         ; Emacs as a Csharp/Unity IDE
    ;; init-collab         ; For collab programming
    ))
