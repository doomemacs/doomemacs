(defconst *big-font (font-spec :family "Inconsolata" :size 18 :antialias t))

(defvar big-mode nil)
(defun toggle-big-mode ()
  (interactive)
  (if big-mode
      (set-frame-font *default-font)
    (set-frame-font *big-font))
  (setq big-mode (not big-mode)))

(provide 'init-present)
;;; init-present.el ends here
