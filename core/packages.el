;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core.el
(package! auto-minor-mode :pin "17cfa1b54800fdef2975c0c0531dad34846a5065")
(package! gcmh :pin "f542908b9ae4405d70fa70f42bd62618c5de4b95")

;; core-ui.el
(package! all-the-icons :pin "1416f37984486a44c6c0cbe0a2c985e82f965b6b")
(package! hide-mode-line :pin "88888825b5b27b300683e662fa3be88d954b1cea")
(package! highlight-numbers :pin "8b4744c7f46c72b1d3d599d4fb75ef8183dee307")
(package! rainbow-delimiters :pin "5125f4e47604ad36c3eb4706310fcafac729ca8c")
(package! restart-emacs :pin "9aa90d3df9e08bc420e1c9845ee3ff568e911bd9")

;; core-editor.el
(package! better-jumper :pin "6d240032ca213ccb3347e25f26c29b6822bf03a7")
(package! dtrt-indent :pin "48221c928b72746d18c1e284c45748a0c2f1691f")
(package! helpful :pin "e511e8dbd32a8b8423f07178f0ea7c1ecfc63935")
(when IS-MAC
  (package! ns-auto-titlebar :pin "1efc30d38509647b417f05587fd7003457719256"))
(package! pcre2el :pin "0b5b2a2c173aab3fd14aac6cf5e90ad3bf58fa7d")
(package! smartparens :pin "9449ae08593180ba99e4517897e8e825d3c422a8")
(package! so-long
  :built-in 'prefer ; included in Emacs 27+
  ;; REVIEW so-long is slated to be published to ELPA eventually, but until then
  ;;        I've created my own mirror for it because git.savannah.gnu.org runs
  ;;        on a potato.
  :recipe (:host github :repo "hlissner/emacs-so-long")
  :pin "ed666b0716f60e8988c455804de24b55919e71ca")
(package! undo-tree :pin "1d91157366d1dcae889057d58526a5bd36e3febe")
(package! ws-butler
  ;; Use my fork of ws-butler, which has a few choice improvements and
  ;; optimizations (the original has been abandoned).
  :recipe (:host github :repo "hlissner/ws-butler")
  :pin "e4430d3778a1a11cc4d4770ce8d070ba71d38f07")
(unless IS-WINDOWS
  (package! xclip :pin "88003b782e0a60eab1c8a2fd8b7f140fb2328271"))

;; core-projects.el
(package! projectile :pin "1e7b37f0ae07a6b4ac1b1a5f0e5422cfcb8e1c55")

;; core-keybinds.el
(package! general :pin "f6e928622d78d927c7043da904782ed7160ea803")
(package! which-key :pin "1e3640e48c31f8062f018b5fc84acad696a0ea2a")
