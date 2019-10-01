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
  ;; REVIEW so-long is slated to be published to ELPA eventually, but until then
  ;; I've created my own mirror for it because git.savannah.gnu.org runs on a
  ;; potato.
  :recipe (:host github :repo "hlissner/emacs-so-long"))
(package! osx-clipboard :ignore (not IS-MAC))
(package! undo-tree
  ;; NOTE The version of undo-tree published to ELPA is over 5 years old and
  ;; missing some fixes. Just use the maintainer's repo directly.
  :recipe (:host nil :repo "http://www.dr-qubit.org/git/undo-tree.git"))
(package! ws-butler)
(package! xclip :ignore (not IS-LINUX))

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! general)
(package! which-key)
