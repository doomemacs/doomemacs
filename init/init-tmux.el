(defun my--tmux-send (command)
  (shell-command (format "tmux send-keys %s" command)))

(after "evil"
  (evil-define-interactive-code "<tmux>"
    "Ex tmux argument (a mix between <sh> <f> and <fsh>)"
    :ex-arg shell
    (list (when (evil-ex-p) (evil-ex-file-arg))))

  (evil-ex-define-cmd "t" 'ex:tmux-run)
  (evil-define-command ex:tmux-run (&optional command bang)
    "Sends input to tmux. Use `bang' to append to tmux"
    (interactive "<tmux><!>")
    (my--tmux-send (format (if bang "C-u %s Enter" "%s")
                           (shell-quote-argument command)))
    (when (evil-ex-p)
      (message "[Tmux] %s" command)))

  (evil-ex-define-cmd "tcd" 'ex:tmux-chdir)
  (evil-define-command ex:tmux-chdir (&optional path bang)
    "CDs in tmux using `my--project-root'"
    (interactive "<f><!>")
    (let ((dir (shell-quote-argument
                (if (and path (not (s-blank? path)))
                    (if (file-directory-p path)
                        (file-truename path)
                      (error "Directory doesn't exist %s" path))
                  (my--project-root bang)))))
      (my--tmux-send (format "C-u cd Space %s Enter" (shell-quote-argument dir)))
      (when (evil-ex-p)
        (message "[Tmux] cd %s" dir)))))


(provide 'init-tmux)
;;; init-tmux.el ends here
