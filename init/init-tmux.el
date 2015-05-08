;;; init.tmux.el - settings for interacting with tmux

(defun my--tmux-send (command)
  (shell-command (format "tmux send-keys %s" command)))

(after "evil"
  (evil-define-interactive-code "<tmux>"
    "Ex tmux argument (a mix between <sh> <f> and <fsh>)"
    :ex-arg shell
    (list (when (evil-ex-p) (evil-ex-file-arg))))

  (evil-define-command my:tmux-run (&optional command bang)
    "Sends input to tmux. Use `bang' to append to tmux"
    (interactive "<tmux><!>")
    (my--tmux-send (format (if bang "C-u %s Enter" "%s")
                           (shell-quote-argument command)))
    (when (evil-ex-p)
      (message "[Tmux] %s" command)))

  (evil-define-command my:tmux-chdir (&optional path bang)
    "CDs in tmux using `project-root'"
    (interactive "<f><!>")
    (let ((dir (shell-quote-argument
                (if (and path (not (s-blank? path)))
                    (if (file-directory-p path)
                        (file-truename path)
                      (error "Directory doesn't exist %s" path))
                  (if bang default-directory (project-root))))))
      (my--tmux-send (format "C-u cd Space %s Enter" (shell-quote-argument dir)))
      (when (evil-ex-p)
        (message "[Tmux] cd %s" dir)))))


(provide 'init-tmux)
;;; init-tmux.el ends here
