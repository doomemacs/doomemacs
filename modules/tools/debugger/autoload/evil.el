;; tools/debugger/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload (autoload '+debugger:start "tools/debugger/autoload/evil" nil t)
(evil-define-command +debugger:start (&optional path)
  "Initiate debugger for current major mode"
  (interactive "<f>")
  ;; TODO Add python debugging
  (let ((default-directory (doom-project-root)))
    (pcase major-mode
      ((or 'c-mode 'c++-mode)
       (realgud:gdb (if path (concat "gdb " path))))
      ((or 'ruby-mode 'enh-ruby-mode)
       ;; FIXME
       (doom:repl nil (format "run '%s'" (file-name-nondirectory (or path buffer-file-name)))))
      ('sh-mode
       (let ((shell sh-shell))
         (when (string= shell "sh")
           (setq shell "bash"))
         (pcase shell
           ("bash"
            (realgud:bashdb (if path (concat "bashdb " path))))
           ("zsh"
            (realgud:zshdb (if path (concat "zshdb " path))))
           (_ (user-error "No shell debugger for %s" shell)))))
      ((or 'js-mode 'js2-mode 'js3-mode)
       (realgud:trepanjs))
      ('haskell-mode (haskell-debug))
      (_ (user-error "No debugger for %s" major-mode)))))

;;;###autoload (autoload '+debugger:toggle-breakpoint "tools/debugger/autoload/evil" nil t)
(evil-define-command +debugger:toggle-breakpoint (&optional bang)
  (interactive "<!>")
  (call-interactively (if bang #'realgud:cmd-clear #'realgud:cmd-break)))
