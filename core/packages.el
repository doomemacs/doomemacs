;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core.el
(package! auto-minor-mode)
(package! async)
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
(when IS-MAC
  (package! ns-auto-titlebar))
(package! pcre2el)
(package! smartparens)
(package! so-long
  :built-in 'prefer ; included in Emacs 27+
  ;; REVIEW so-long is slated to be published to ELPA eventually, but until then
  ;;        I've created my own mirror for it because git.savannah.gnu.org runs
  ;;        on a potato.
  :recipe (:host github :repo "hlissner/emacs-so-long"))
(package! undo-tree)
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler"))
(unless IS-WINDOWS
  (package! xclip))

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! general)
(package! which-key)
