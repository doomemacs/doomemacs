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
(package! undo-tree
  ;; Version 0.6.5 is on ELPA which lacks a fix we need, so we install 0.6.6
  ;; from emacsmirror/undo-tree instead.
  :recipe (:host github :repo "emacsmirror/undo-tree"))
(package! ws-butler)

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! general)
(package! which-key)
