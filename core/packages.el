;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core packages
(package! async)
(package! s)
(package! f)
(package! ansi)

;; core-os.el
(when IS-MAC
  (package! exec-path-from-shell)
  (package! osx-clipboard))

;; core-ui.el
(package! highlight-indentation)
(package! highlight-numbers)
(package! nlinum)
(package! rainbow-delimiters)
(package! visual-fill-column)

;; core-popups.el
(package! shackle)

;; core-editor.el
(package! editorconfig)
(package! smartparens)
(package! ace-link)
(package! ace-window)
(package! avy)
(package! command-log-mode)
(package! expand-region)
(package! goto-last-change)
(package! help-fns+)
(package! imenu-anywhere)
(package! imenu-list)
(package! pcre2el)
(package! smart-forward)
(package! wgrep)

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! which-key)
