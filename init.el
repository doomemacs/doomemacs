;;; Emacs for the jaded vimmer
;;
;; Author: Henrik Lissner <henrik@lissner>
;; URL: https://github.com/hlissner/emacs.d
;;
;;; Description:
;;
;; My emacs.d sets out to rustle emacs users' jimmies by making emacs as
;; vim-like as possible.
;;
;;; Code:
(defconst DEBUG-MODE nil)

(defconst my-dir           user-emacs-directory)
(defconst my-modules-dir   (concat my-dir "init/"))
(defconst my-contrib-dir   (concat my-dir "contrib/"))
(defconst my-themes-dir    (concat my-dir "themes/"))
(defconst my-snippets-dir  (concat my-dir "snippets/"))
(defconst my-tmp-dir       (concat my-dir ".cache-" (system-name) "/"))

(defconst *default-theme 'v0)
(defconst *default-font (font-spec :family "Terminus (TTF)" :size 12 :antialias nil))

(add-to-list 'load-path my-modules-dir)
(add-to-list 'load-path my-contrib-dir)
;; Add cask dirs to load-path
(let ((default-directory (expand-file-name (concat ".cask/" emacs-version "/elpa/") my-dir)))
  (normal-top-level-add-subdirs-to-load-path))

;;;; Load Packages ;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)
(mapc 'require
      ;; ls init/{init,my}* | xargs basename | sed -e 's/\..*$//'
      '(core
        core-ui                ; aesthetics
        core-evil              ; evil-mode and its plugins
        core-editor            ; completing the editor

        init-auto-insert       ; for the lazy typist
        init-company           ; see above
        init-fly               ; fly(check|spell)
        init-vc                ; version control gutter + git modes
        init-ido               ; a search engine for your car keys
        init-helm              ; a search engine for your life
        init-project           ; dired, neotree
        init-present           ; for when I need to show off

        init-cc                ; C/C++/Obj-C madness
        ;; init-cscope
        init-csharp
        ;; init-eshell
        ;; init-go
        init-java              ; the poster child for carpal tunnel syndome
        init-js                ; alert("not java, javascript!")
        init-lua               ; one-based indices? One-based indices.
        init-org               ; for fearless [organized] leader
        init-php               ; making php less painful to work with
        init-python            ; beautiful is better than ugly
        init-regex             ; /^[^\s](meaning)[^\n]*/
        init-ruby              ; <3
        init-scss              ; @include magic;
        init-sh                ; #!/bin/bash_your_head_in
        init-swift             ; yay, emoji variables!
        init-text              ; I got nothing...
        init-tmux
        ;; init-rust
        ;; init-R
        init-web               ; For the 2.0'er
        init-workgroups        ; session management I can understand
        init-yasnippet         ; type for me

        my-defuns
        my-bindings
        my-commands
        my-settings
        ))

;; I've created a monster!
