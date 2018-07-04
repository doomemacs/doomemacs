;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core-os.el
(when IS-MAC
  (package! exec-path-from-shell)
  (package! osx-clipboard))

;; core-ui.el
(package! all-the-icons)
(package! hide-mode-line)
(package! highlight-indentation)
(package! highlight-numbers)
(unless (boundp 'display-line-numbers)
  (package! nlinum)
  (package! nlinum-hl)
  (package! nlinum-relative))
(package! rainbow-delimiters)
(package! visual-fill-column)
(package! restart-emacs)

;; core-editor.el
(package! ace-link)
(package! ace-window)
(package! avy)
(package! command-log-mode)
(package! dtrt-indent)
(package! expand-region)
(package! helpful)
(package! pcre2el)
(package! smartparens)
(package! undo-tree)

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! which-key)
(package! hydra)

;; autoload/debug.el
(package! esup)

;; autoload/test.el
(package! buttercup)
