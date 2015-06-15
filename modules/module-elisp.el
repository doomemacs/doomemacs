;;; module-elisp --- all things emacs lisp
;; see lib/elisp-defuns.el

(add-hook! emacs-lisp-mode 'turn-on-eldoc-mode)

;; [pedantry intensifies]
(defadvice emacs-lisp-mode (after emacs-lisp-mode-rename-modeline activate)
  (setq mode-name "Elisp"))

;; Real go-to-definition for elisp
(bind! :map emacs-lisp-mode-map
       :m "gd" 'narf/elisp-find-function-at-pt
       :m "gD" 'narf/elisp-find-function-at-pt-other-window)

(provide 'module-elisp)
;;; module-elisp.el ends here
