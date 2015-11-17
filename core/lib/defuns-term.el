;;; defuns-tmux.el

(defun narf--send-to-tmux (command)
  (shell-command (format "tmux send-keys %s" command)))

(evil-define-interactive-code "<term>"
  "Ex tmux argument (a mix between <sh> <f> and <fsh>)"
  :ex-arg shell
  (list (when (evil-ex-p) (evil-ex-file-arg))))

;;;###autoload (autoload 'narf:send-to-tmux "defuns-term" nil t)
(evil-define-command narf:send-to-tmux (command &optional bang)
  "Sends input to tmux. Use `bang' to append to tmux"
  (interactive "<term><!>")
  (narf--send-to-tmux (format (if bang "%s" "C-u %s Enter")
                              (shell-quote-argument command)))
  (when (evil-ex-p)
    (message "[Tmux] %s" command)))

(provide 'defuns-tmux)
;;; defuns-tmux.el ends here
