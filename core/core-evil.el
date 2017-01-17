;;; core-evil.el --- Come to the dark side, we have cookies

;; TODO Document

(defvar doom-evil-leader ","
  "docstring")

(defvar doom-evil-localleader "\\"
  "docstring")


;;
;; Packages
;;

(package! evil :demand t
  :init
  (setq evil-magic t
        evil-want-C-u-scroll t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-want-visual-char-semi-exclusive t
        evil-ex-search-vim-style-regexp t
        evil-ex-interactive-search-highlight 'selected-window
        evil-echo-state nil
        evil-ex-substitute-global t
        evil-insert-skip-empty-lines t
        evil-want-fine-undo nil

        evil-normal-state-tag    "N"
        evil-insert-state-tag    "I"
        evil-visual-state-tag    "V"
        evil-emacs-state-tag     "E"
        evil-operator-state-tag  "O"
        evil-motion-state-tag    "M"
        evil-replace-state-tag   "R")

  :config
  (evil-mode +1)
  (evil-select-search-module 'evil-search-module 'evil-search))


;;
;; Autoloaded Packages
;;

(provide 'core-evil)
;;; core-evil.el ends here
