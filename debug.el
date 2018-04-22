;;; debug.el -*- lexical-binding: t; -*-

;; To test something in a blank, vanilla Emacs session (Emacs -Q) load me:
;;
;;   emacs -Q -l debug.el

(setq user-emacs-directory (file-name-directory load-file-name)
      package--init-file-ensured t
      package-user-dir (expand-file-name ".local/packages/elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;; Then you can test packages in isolation here...
