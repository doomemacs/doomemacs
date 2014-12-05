;;; Emacs for the jaded vimmer
;;
;; Author: Henrik Lissner <henrik@lissner>
;; URL: https://github.com/hlissner/emacs.d
;;
;; My emacs.d, which sets out to rustle emacs users' jimmies by making
;; emacs as vim-like as possible.
;;
;;; Description:
;;
;; Naming conventions:
;;    * my--<defun-name>  ; interal defuns, meant for use via elisp
;;    * my-<defun-name>   ; interactive command, can be used via M-x
;;    * my.<defun-name>   ; commands with buffer side-effects (for keybinds)
;;    * my:<defun-name>   ; defuns meant to be used from Ex mode
;;    * my/<defun-name>   ; defuns meant to be used from Ex mode
;;    * *<defun/var-name> ; for altering the visual state
;;
;;
;;; Code:

(defconst *debug-mode nil)

(defconst my-dir           user-emacs-directory)
(defconst my-init-dir      (concat my-dir "init/"))
(defconst my-elisp-dir     (concat my-dir "elisp/"))
(defconst my-themes-dir    (concat my-dir "themes/"))
(defconst my-snippets-dir  (concat my-dir "snippets/"))
(defconst my-ac-dicts-dir  (concat my-dir "ac-dict/"))
(defconst my-tmp-dir       (concat my-dir ".cache/"))

(defconst *dark-theme   'brin)
(defconst *light-theme  'github) ; wtb better light theme...

(defconst *default-font "Ubuntu Mono")
(defconst *default-font-size (if (eq system-name "ganymede.local") 12 14))

(defconst *presentation-font *default-font)
(defconst *presentation-font-size 18)

(add-to-list 'load-path my-init-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just the... bear necessities...
(defconst my-modules
  ;; ls -1 init/{init,my}* | xargs basename | sed -e 's/\..*$//'
  '(core

    ;; init-auto-complete
    init-company
    init-auto-insert
    ;; init-cpp
    ;; init-cscope
    ;; init-csharp
    init-dev
    init-elisp
    ;; init-eshell
    init-fly
    init-git
    ;; init-go
    init-helm
    init-ido
    init-java
    init-lua
    init-org
    init-project            ; project management settings & tools
    init-projectile
    init-python
    init-regex
    init-ruby
    init-sh
    ;; init-swift
    init-text
    init-tmux
    init-webdev
    init-yasnippet

    my-bindings
    my-settings
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load them in
(setq after-init-hook '((lambda() (mapc 'require my-modules))))

;; I've created a monster!
