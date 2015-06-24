;;; module-elisp --- all things emacs lisp
;; see lib/elisp-defuns.el

(add-hook! emacs-lisp-mode 'turn-on-eldoc-mode)

;; [pedantry intensifies]
(defadvice emacs-lisp-mode (after emacs-lisp-mode-rename-modeline activate)
  (setq mode-name "Elisp"))

(defun narf-elisp-auto-compile ()
  (when (narf/is-recompilable-p)
    (narf:compile-el)))

(add-hook! emacs-lisp-mode
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  (add-hook 'after-save-hook 'narf-elisp-auto-compile nil t))

;; Real go-to-definition for elisp
(bind! :map emacs-lisp-mode-map
       :m "gd" 'narf/elisp-find-function-at-pt
       :m "gD" 'narf/elisp-find-function-at-pt-other-window)

(provide 'module-elisp)
;;; module-elisp.el ends here
