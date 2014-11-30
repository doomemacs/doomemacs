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
(setq use-package-verbose t) ; for debug purposes

(require 'cask)
(cask-initialize)

(setq user-mail-address "henrik@lissner.net")

(defconst *dir           (file-name-directory load-file-name))
(defconst *init-dir      (concat *dir "init/"))
(defconst *themes-dir    (concat *dir "themes/"))
(defconst *elisp-dir     (concat *dir "elisp/"))
(defconst *snippets-dir  (concat *dir "snippets/"))
(defconst *ac-dicts-dir  (concat *dir "ac-dict/"))
(defconst *tmp-dir       (concat *dir "tmp/"))

(defconst *theme  'brin)
(defconst *font
  (if (string-equal system-name "ganymede.local")
      "Ubuntu Mono-12"      ; Use smaller font on my laptop
    "Ubuntu Mono-14"))      ; And larger font everywhere else

(add-to-list 'load-path *init-dir)

;; Just the... bear necessities...
(mapc 'require
  '(core
    core-defuns             ; Defun library
    core-editor             ; Global editor behavior (w/ evil)
    core-ui                 ; User interface layout & behavior
    core-osx                ; Mac-specific config

    ;; Essential plugins & modules
    init-ido                ; Ido setup
    init-project            ; Project nav+search tools (projectile, helm, ag)
    init-snippets           ; Snippet engine
    init-git                ; GIT tools/settings
    init-fly                ; Syntax & spell checker
    init-auto-complete      ; Auto-complete engine
    init-auto-insert        ; File auto-insert templates
    init-cscope             ; Global code indexing

    ;; Modes & environments
    init-text               ; Plain text editing (markdown, text)
    init-sh                 ; Shell script editing (sh, zsh)
    init-org                ; Org-mode: personal gtd/notes
    init-dev                ; Generic dev tools & environment for all programming
    init-ruby
    init-python
    ;;init-php
    init-webdev             ; Environment for webdev (SCSS, PHP, Rails, Javascript)
    init-love               ; Love.app gamedev
    init-cpp                ; C++ gamedev
    init-java               ; Java-specific settings (including eclim)
    ;; init-go                 ; Go-lang
    ;; init-swift              ; iOS/Mac dev environment for swift
    ;; init-csharp             ; Emacs as a Csharp/Unity IDE

    ;; My homebaked packages
    my-commands             ; Ex commands & evil operators/commands
    my-coderunner           ; Code/REPL runners

    ;; Personal settings (must be last!)
    my-settings             ; Any other custom settings
    my-keymaps              ; My keybindings
    ))

;; I've created a monster!
