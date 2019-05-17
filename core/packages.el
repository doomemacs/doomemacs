;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core.el
(package! dotenv-mode)

;; core-os.el
(if (not IS-MAC)
    (package! xclip)
  (package! osx-clipboard)
  (package! ns-auto-titlebar))

;; core-ui.el
(package! all-the-icons)
(package! hide-mode-line)
(package! highlight-numbers)
(unless (locate-library "display-line-numbers")
  (package! nlinum)
  (package! nlinum-hl)
  (package! nlinum-relative))
(package! rainbow-delimiters)
(package! restart-emacs)

;; core-editor.el
(package! better-jumper)
(package! command-log-mode)
(package! dtrt-indent)
(package! helpful)
(package! pcre2el)
(package! smartparens)
(package! undo-tree)
(package! ws-butler)

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! general)
(package! which-key)
(package! hydra)

;; core-packages.el
(package! gnu-elpa-keyring-update)

;; autoload/debug.el
(package! esup)

;; cli/test.el
(package! buttercup)
