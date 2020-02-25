;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core.el
(package! auto-minor-mode :pin "17cfa1b548")
(package! gcmh :pin "8867533a73")

;; core-ui.el
(package! all-the-icons :pin "1416f37984")
(package! hide-mode-line :pin "88888825b5")
(package! highlight-numbers :pin "8b4744c7f4")
(package! rainbow-delimiters :pin "5125f4e476")
(package! restart-emacs :pin "9aa90d3df9")

;; core-editor.el
(package! better-jumper :pin "6d240032ca")
(package! dtrt-indent :pin "48221c928b")
(package! helpful :pin "c54e9ddbd6")
(when IS-MAC
  (package! ns-auto-titlebar :pin "1efc30d385"))
(package! pcre2el :pin "0b5b2a2c17")
(package! smartparens :pin "be8d5c9a63")
(package! so-long
  :built-in 'prefer ; included in Emacs 27+
  ;; REVIEW so-long is slated to be published to ELPA eventually, but until then
  ;;        I've created my own mirror for it because git.savannah.gnu.org runs
  ;;        on a potato.
  :recipe (:host github :repo "hlissner/emacs-so-long")
  :pin "ed666b0716")
(package! undo-tree :pin "5b6df03781")
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler")
  :pin "e4430d3778")
(unless IS-WINDOWS
  (package! xclip :pin "d022cf947d"))

;; core-projects.el
(package! projectile :pin "341150c0e7")

;; core-keybinds.el
(package! general :pin "f6e928622d")
(package! which-key :pin "7b068f3e95")

;; autoload/cache.el
(package! persistent-soft :pin "a1e0ddf2a1")
