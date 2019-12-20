;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core.el
(package! auto-minor-mode)
(package! gcmh)

;; core-ui.el
(package! all-the-icons)
(package! hide-mode-line)
(package! highlight-numbers)
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
(package! xclip)

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! general)
(package! which-key)
