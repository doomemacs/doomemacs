;; OSX-specific functionality
(when (is-osx)

	;; GNU Emacs won't respect the login shell's PATH on mac, it seems,
	;; this is necessary to fix the problem! (Thanks to lunaryorn @
	;; <https://github.com/flycheck/flycheck/issues/438#issuecomment-49543748>)
	(require-package 'exec-path-from-shell)
	(exec-path-from-shell-initialize)

    ;; Ignore .DS_Store files with ido mode
    (add-to-list 'ido-ignore-files ".DS_Store")

	;; Use a shared clipboard
	(add-hook 'before-make-frame-hook
			  (lambda() (setq x-select-enable-clipboard t)))

    ;; Curse you Lion-esque fullscreen mode!
    (setq ns-use-native-fullscreen nil)
    ;; Don't open files from the workspace in a new frame
    (setq ns-pop-up-frames nil)

    ;; Send current file to OSX apps
    (defun open-file-with (path &optional appName)
      (if (not (string= "" appName))
          (setq appName (concat "-a " appName ".app")))
      (shell-command (concat "open " appName " " path)))

    (defun open-with (appName)
      (interactive)
      (open-file-with (buffer-file-name) appName))

    (defun send-to-transmit ()      (interactive) (open-with "Transmit"))
    (defun send-to-launchbar ()     (interactive) (open-with "LaunchBar"))
    (defun send-dir-to-launchbar () (interactive) (open-file-with default-directory "LaunchBar"))
    (defun send-dir-to-finder ()    (interactive) (open-file-with default-directory "Finder"))
)

;;
(provide 'core-osx)
