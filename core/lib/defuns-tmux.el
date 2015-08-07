;;; defuns-tmux.el

(defun narf--tmux-send (command)
  (shell-command (format "tmux send-keys %s" command)))

(evil-define-interactive-code "<tmux>"
  "Ex tmux argument (a mix between <sh> <f> and <fsh>)"
  :ex-arg shell
  (list (when (evil-ex-p) (evil-ex-file-arg))))

;;;###autoload (autoload 'narf:tmux-run "defuns-tmux" nil t)
(evil-define-command narf:tmux-run (&optional command bang)
  "Sends input to tmux. Use `bang' to append to tmux"
  (interactive "<tmux><!>")
  (narf--tmux-send (format (if bang "C-u %s Enter" "%s")
                          (shell-quote-argument command)))
  (when (evil-ex-p)
    (message "[Tmux] %s" command)))

;;;###autoload (autoload 'narf:tmux-chdir "defuns-tmux" nil t)
(evil-define-command narf:tmux-chdir (&optional path bang)
  "CDs in tmux using `narf/project-root'"
  (interactive "<f><!>")
  (let ((dir (shell-quote-argument
              (if (and path (not (s-blank? path)))
                  (if (file-directory-p path)
                      (file-truename path)
                    (error "Directory doesn't exist %s" path))
                (if bang default-directory (narf/project-root))))))
    (narf--tmux-send (format "C-u cd Space %s Enter" (shell-quote-argument dir)))
    (when (evil-ex-p)
      (message "[Tmux] cd %s" dir))))

(provide 'defuns-tmux)
;;; defuns-tmux.el ends here
