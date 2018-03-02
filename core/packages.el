;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core-os.el
(package! exec-path-from-shell :ignore (not IS-MAC))
(package! osx-clipboard        :ignore (not IS-MAC))

;; core-ui.el
(package! all-the-icons)
(package! fringe-helper)
(package! hide-mode-line)
(package! highlight-indentation)
(package! highlight-numbers)
(unless (boundp 'display-line-numbers)
  (package! nlinum)
  (package! nlinum-hl)
  (package! nlinum-relative))
(package! rainbow-delimiters)
(package! visual-fill-column)

;; core-popups.el
(package! shackle)

;; core-editor.el
(package! ace-link)
(package! ace-window)
(package! avy)
(package! command-log-mode)
(package! editorconfig)
(package! expand-region)
(package! helpful)
(package! pcre2el)
(package! smart-forward)
(package! smartparens)
(package! undo-tree)

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! which-key)
(package! hydra)
