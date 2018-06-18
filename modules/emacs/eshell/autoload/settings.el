;;; emacs/eshell/autoload/settings.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-eshell-alias! (&rest aliases)
  "Define aliases for eshell."
  (or (cl-evenp (length aliases))
      (signal 'wrong-number-of-arguments (list 'even (length aliases))))
  (after! eshell
    (while aliases
      (map-put +eshell-aliases (pop aliases) (list (pop aliases))))
    (when (boundp 'eshell-command-aliases-list)
      (if +eshell--default-aliases
          (setq eshell-command-aliases-list
                (append +eshell--default-aliases
                        +eshell-aliases))
        (setq eshell-command-aliases-list +eshell-aliases)))))
