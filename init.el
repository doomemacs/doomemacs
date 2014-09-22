;;; Emacs for the jaded vimmer
;;
;; Author: Henrik Lissner <henrik@lissner>
;; URL: https://github.com/hlissner/emacs.d
;;
;; My emacs.d, which sets out to rustle emacs users' jimmies by making
;; emacs as vim-like as possible.
;;
;;; Code:

;; instead of /
(cd "~")
;; (setq use-package-verbose t) ; for debug purposes

(require 'cask)
(cask-initialize)

(defconst *dir           (file-name-directory load-file-name))
(defconst *init-dir      (concat *dir "init/"))
(defconst *themes-dir    (concat *dir "themes/"))
(defconst *elisp-dir     (concat *dir "elisp/"))
(defconst *snippets-dir  (concat *dir "snippets/"))
(defconst *ac-dicts-dir  (concat *dir "ac-dict"))
(defconst *tmp-dir       "/tmp/emacs/")

(defconst *theme  'brin)
(defconst *font   "Inconsolata-16")

(add-to-list 'load-path *init-dir)

;; Just the... bear necessities...
(mapc 'require
  '(core
    my-defuns               ; Personal library

    ;; Tailoring emacs
    core-editor             ; Internal config for global editor behavior
    core-ui                 ; User interface layout & behavior
    core-osx                ; Mac-specific config

    ;; Plugins & modules
    init-ido                ; Ido setup
    init-project            ; Project navigation tools & settings
    init-ac                 ; Auto-complete engine & settings
    init-snippets           ; Snippet engine
    init-git                ; GIT tools/settings
    init-fly                ; Syntax & spell checker

    ;; Modes & environments
    init-text               ; Plain text editing (markdown, text)
    init-org                ; Org-mode: personal gtd/notes
    init-dev                ; Generic dev tools & environment for all programming
    init-ruby
    init-python
    init-webdev             ; Environment for webdev (SCSS, PHP, Rails, Jekyll)
    init-love               ; Love.app gamedev
    init-cpp                ; C++ gamedev
    init-eclim              ; Integration into eclipse (for Java)
    ;; init-csharp             ; Emacs as a Csharp/Unity IDE

    my-settings             ; Any other custom settings
    my-commands             ; Interactive defuns & evil operators/commands
    my-keymaps              ; My keybindings
    ))

;; (require 'server)
;; (unless (server-running-p) (server-start))

;; I've created a monster!
