;;; debug.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; load me after running Emacs -Q:
;;
;;   (load-file (concat user-emacs-directory "core/debug.el"))
;;
;; then you can test packages in isolation.

(setq user-emacs-directory (expand-file-name "../" (file-name-directory load-file-name))
      package--init-file-ensured t
      package-user-dir (expand-file-name ".local/packages/elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))

(package-initialize)
