;;; lisp/init.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; :doom is now treated like a normal module, and this is its (temporary) init
;; file, which will be removed once we've resolved our `use-package' dependency
;; (which will soon be moved to its own module), then these will be returned to
;; the profile init file.
;;
;;; Code:

(doom-require 'doom-keybinds)
(doom-require 'doom-ui)
(doom-require 'doom-projects)
(doom-require 'doom-editor)

;; Ensure .dir-locals.el in $EMACSDIR and $DOOMDIR are always respected
(add-to-list 'safe-local-variable-directories doom-emacs-dir)
(add-to-list 'safe-local-variable-directories doom-user-dir)

;;; Support for Doom-specific file extensions
(add-to-list 'auto-mode-alist '("/\\.doom\\(?:project\\|module\\|profile\\)?\\'" . lisp-data-mode))
(add-to-list 'auto-mode-alist '("/\\.doomrc\\'" . emacs-lisp-mode))

;;; init.el ends here
