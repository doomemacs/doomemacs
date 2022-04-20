;;; os/macos/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +macos-open-with (&optional app-name path)
  "Send PATH to APP-NAME on OSX."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (format "open %s"
                          (if app-name
                              (format "-a %s '%s'" (shell-quote-argument app-name) path)
                            (format "'%s'" path)))))
    (message "Running: %s" command)
    (shell-command command)))

;;;###autoload
(defun +macos--enable-in-new-window (bool)
  (let ((command (format "defaults write com.googlecode.iterm2 OpenFileInNewWindows -bool %s" (if bool
											       "true"
											     "false"))))
    (message "Running: %s" command)
    (shell-command command)))

;;;###autoload
(defun +macos--read-new-window-value ()
  (let ((command "defaults read com.googlecode.iterm2 OpenFileInNewWindows"))
    (message "Running: %s" command)
    (let ((value (shell-command command)))
      (if (eq value 1)
	  t
	nil))))

;;;###autoload
(defmacro +macos--open-with-iterm (id &optional app dir new)
  `(defun ,(intern (format "+macos/%s" id)) ()
     (interactive)
     (let ((original-value (+macos--read-new-window-value)))
       (if (and ,new (not original-value))
		(+macos--enable-in-new-window t))
       (+macos-open-with ,app ,dir)
       (if (and ,new (not original-value))
		(+macos--enable-in-new-window nil)))))

;;;###autoload
(defmacro +macos--open-with (id &optional app dir)
  `(defun ,(intern (format "+macos/%s" id)) ()
     (interactive)
     (+macos-open-with ,app ,dir)))

;;;###autoload (autoload '+macos/open-in-default-program "os/macos/autoload" nil t)
(+macos--open-with open-in-default-program)

;;;###autoload (autoload '+macos/reveal-in-finder "os/macos/autoload" nil t)
(+macos--open-with reveal-in-finder "Finder" default-directory)

;;;###autoload (autoload '+macos/reveal-project-in-finder "os/macos/autoload" nil t)
(+macos--open-with reveal-project-in-finder "Finder"
                   (or (doom-project-root) default-directory))

;;;###autoload (autoload '+macos/send-to-transmit "os/macos/autoload" nil t)
(+macos--open-with send-to-transmit "Transmit")

;;;###autoload (autoload '+macos/send-cwd-to-transmit "os/macos/autoload" nil t)
(+macos--open-with send-cwd-to-transmit "Transmit" default-directory)

;;;###autoload (autoload '+macos/send-to-launchbar "os/macos/autoload" nil t)
(+macos--open-with send-to-launchbar "LaunchBar")

;;;###autoload (autoload '+macos/send-project-to-launchbar "os/macos/autoload" nil t)
(+macos--open-with send-project-to-launchbar "LaunchBar"
                   (or (doom-project-root) default-directory))

;;;###autoload (autoload '+macos/open-in-iterm "os/macos/autoload" nil t)
(+macos--open-with-iterm open-in-iterm "iTerm" default-directory)

;;;###autoload (autoload '+macos/open-in-iterm-new-window "os/macos/autoload" nil t)
(+macos--open-with-iterm open-in-iterm-new-window "iTerm" default-directory t)
