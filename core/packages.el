;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core-os.el
(when IS-MAC
  (package! exec-path-from-shell)
  (package! osx-clipboard)
  (package! ns-auto-titlebar))

;; core-ui.el
(package! all-the-icons
  :recipe (:fetcher github :repo "ubolonton/all-the-icons.el"
           :branch "font-lock-fix" :files (:defaults "data")))
(package! hide-mode-line)
(package! highlight-indentation)
(package! highlight-numbers)
(package! highlight-escape-sequences
  :recipe (:fetcher github :repo "hlissner/highlight-escape-sequences"))
(unless (locate-library "display-line-numbers")
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
(package! ws-butler)

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! general)
(package! which-key)
(package! hydra)

;; autoload/debug.el
(package! esup)

;; autoload/test.el
(package! buttercup)
