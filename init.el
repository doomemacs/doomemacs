;;; Emacs for the jaded vimmer
;;
;; Author: Henrik Lissner <henrik@lissner>
;; URL: https://github.com/hlissner/emacs.d
;;
;;; Description:
;;
;; My emacs.d, which sets out to rustle emacs users' jimmies by making
;; emacs as vim-like as possible.
;;
;; Naming conventions:
;;    * my--<defun-name>  ; interal defuns, meant for use via elisp
;;    * my-<defun-name>   ; interactive command, can be used via M-x
;;    * my.<defun-name>   ; commands with buffer side-effects (for keybinds)
;;    * my:<defun-name>   ; defuns meant to be used from Ex mode
;;    * *<defun/var-name> ; for altering the visual state
;;
;;; Code:

(defconst *debug-mode nil)

(defconst my-dir           user-emacs-directory)
(defconst my-core-dir      (concat my-dir "core/"))
(defconst my-modules-dir   (concat my-dir "modules/"))
(defconst my-personal-dir  (concat my-dir "my/"))
(defconst my-elisp-dir     (concat my-dir "lib/"))
(defconst my-themes-dir    (concat my-dir "themes/"))
(defconst my-snippets-dir  (concat my-dir "snippets/"))
(defconst my-tmp-dir       (concat my-dir ".cache/"))

(defconst *dark-theme   'brin)
(defconst *light-theme  'github) ; wtb better light theme...

(defconst *default-font "Ubuntu Mono")
(defconst *default-font-size (if (eq system-name "ganymede.local") 12 14))

(defconst *presentation-font *default-font)
(defconst *presentation-font-size 18)

(add-to-list 'load-path my-core-dir)
(add-to-list 'load-path my-modules-dir)
(add-to-list 'load-path my-personal-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just the... bear necessities...
(defconst my-modules
  ;; ls -1 init/{init,my}* | xargs basename | sed -e 's/\..*$//'
  '(core

    ;; init-auto-complete
    init-auto-insert       ; for the lazy typis
    init-company           ; see above
    init-cc                ; C/C++/Obj-C madness
    ;; init-cscope
    ;; init-csharp
    init-dev               ; general dev tools/settings
    init-elisp             ; emacs lisp
    ;; init-eshell
    init-floobits          ; when I'm feeling lonely
    init-fly               ; fly(check|spell)
    init-git               ; git-gutter + modes
    ;; init-go
    init-helm              ; when you forget where you put your constellation
    init-ido               ; when you forget where you put your keys
    init-java              ; the mascot language of carpal tunnel syndome
    init-js                ; alert("Oh, sure dude, I know java")
    init-lua               ; zero-based indices? Zero-based indices.
    init-org               ; for fearless leader (who is organized)
    init-php               ; making php less painful to work with
    init-project           ; project tools - dired, perspective, neotree
    init-projectile        ; when you forget where you put your house
    init-python            ; beautiful is better than ugly
    init-regex             ; /^[^\s](meaning)[^\n]*life/
    init-ruby              ; I frakking love ruby
    init-scss              ; @include magic;
    init-sh                ; #!/bin/bash_your_head_in
    init-swift             ; ever wanted to name a variable an emoticon?
    init-text              ; I got nothing...
    init-tmux
    init-web
    init-yasnippet         ; type for me

    my-bindings
    my-settings
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load them in
(setq after-init-hook '((lambda() (mapc 'require my-modules))))

;; I've created a monster!
