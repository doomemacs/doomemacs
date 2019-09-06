;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core.el
(package! dotenv-mode)
(package! auto-minor-mode)

;; core-ui.el
(package! all-the-icons)
(package! hide-mode-line)
(package! highlight-numbers)
;; Some early 26.x builds of Emacs do not have `display-line-numbers' yet, so
;; check for it instead of Emacs' version.
(unless (locate-library "display-line-numbers")
  (package! nlinum)
  (package! nlinum-hl)
  (package! nlinum-relative))
(package! rainbow-delimiters)
(package! restart-emacs)

;; core-editor.el
(package! better-jumper)
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
(package! xclip :ignore (not IS-LINUX))

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! general)
(package! which-key)
