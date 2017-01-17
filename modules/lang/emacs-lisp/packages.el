;;; lang/emacs-lisp/packages.el

(associate! emacs-lisp-mode :match "\\(/Cask\\|\\.elc?\\(.gz\\)?\\)$")

(package! highlight-quoted :commands highlight-quoted-mode)

(package! auto-compile :commands auto-compile-on-save-mode)

(package! slime)
