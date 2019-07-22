;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core.el
(package! dotenv-mode)
(package! auto-minor-mode)

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
(package! ns-auto-titlebar :ignore (not IS-MAC))
(package! pcre2el)
(package! smartparens)
(package! so-long
  :built-in 'prefer
  :recipe (:repo "https://git.savannah.gnu.org/git/so-long.git"))
(package! osx-clipboard :ignore (not IS-MAC))
(package! undo-tree)
(package! ws-butler)
(package! xclip :ignore IS-LINUX)

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! general)
(package! which-key)

;; autoload/debug.el
(package! esup)
