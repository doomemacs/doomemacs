;;; module-sh.el --- description

(after! sh-script
  (define-repl! sh-mode narf-inf-shell)
  (add-hook! sh-mode 'flycheck-mode)

  (defun sh-extra-font-lock--is-in-double-quoted-string ()
    "Non-nil if point in inside a double-quoted string."
    (let ((state (syntax-ppss)))
      (eq (nth 3 state) ?\")))

  (defun sh-extra-font-lock--match-var-in-double-quoted-string (limit)
    "Search for variables in double-quoted strings bounded by LIMIT."
    (let (res)
      (while
          (and (setq res
                     (re-search-forward
                      "\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)"
                      limit t))
               (not (sh-extra-font-lock--is-in-double-quoted-string))))
      res))

  (defvar sh-extra-font-lock--keywords
    '((sh-extra-font-lock--match-var-in-double-quoted-string
       (2 font-lock-variable-name-face prepend))))

  (defun sh-extra-font-lock-activate ()
    "Activate sh-extra-font-lock."
    (interactive)
    (font-lock-add-keywords nil sh-extra-font-lock--keywords)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (when font-lock-mode
        (with-no-warnings
          (font-lock-fontify-buffer)))))

  (add-hook 'sh-mode-hook 'sh-script-extra-font-lock-activate)

  ;; [pedantry intensifies]
  (defadvice sh-mode (after sh-mode-rename-modeline activate)
    (setq mode-name "sh"))

  (defun narf-inf-shell ()
    (let* ((dest-sh (symbol-name sh-shell))
           (sh-shell-file dest-sh))
      (sh-shell-process t)
      (with-current-buffer "*shell*"
        (rename-buffer (format "*shell [%s]*" dest-sh))))))

(provide 'module-sh)
;;; module-sh.el ends here
