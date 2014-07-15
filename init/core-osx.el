;; OSX-specific functionality
(if (is-osx) (progn

    ;; Ignore .DS_Store files with ido mode
    (add-to-list 'ido-ignore-files "\\.DS_Store")

    (if window-system (progn
        (setq ns-use-native-fullscreen nil)
        (global-set-key (kbd "s-<f12>") 'toggle-frame-fullscreen)

        (x-focus-frame nil)
        ;; Don't open files from the workspace in a new frame
        (setq ns-pop-up-frames nil)
    ))

    ;; Send current file to OSX apps
    (defun open-file-with (path &optional appName)
      (if (not (string= "" appName))
          (setq appName (concat "-a " appName ".app")))
      (shell-command (concat "open " appName " " path)))

    (defun open-with (appName)
      (interactive)
      (open-file-with (buffer-file-name) appName))

    (defun send-to-transmit () (open-with "Transmit"))
    (defun send-to-launchbar () (open-with "LaunchBar"))
    (defun send-dir-to-launchbar () (open-file-with default-directory "LaunchBar"))
    (defun send-dir-to-finder () (open-file-with default-directory "Finder"))
    (defun open-in-terminal () (ansi-term "/bin/zsh"))

    (after 'evil
           (gmap (kbd "s-/") 'evilnc-comment-or-uncomment-lines)
           (gmap (kbd "s-w") 'evil-window-delete)

           ;; Fast scrolling
           (nmap (kbd "s-j") "jjjjj")
           (nmap (kbd "s-k") "kkkkk")

           ;; Newlines from insert mode
           (imap (kbd "<s-return>") 'evil-open-below)
           (imap (kbd "<S-s-return>") 'evil-open-above)

           ;; Fix OSX text navigation shortcuts
           (imap (kbd "<s-left>") 'move-beginning-of-line)
           (imap (kbd "<s-right>") 'move-end-of-line)
           (imap (kbd "<s-backspace>") 'backward-kill-line)
           )
))

;;
(provide 'core-osx)
