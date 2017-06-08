;;; tools/macos/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +macos-open-with (&optional app-name path)
  "Send PATH to APP-NAME on OSX."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (eq major-mode 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (format "open %s"
                          (if app-name
                              (format "-a %s '%s'" (shell-quote-argument app-name) path)
                            (format "'%s'" path)))))
    (message "Running: %s" command)
    (shell-command command)))

(defmacro +macos!open-with (id &optional app dir)
  `(defun ,(intern (format "+macos/%s" id)) ()
     (interactive)
     (+macos-open-with ,app ,dir)))

;;;###autoload (autoload '+macos/open-in-default-program "tools/macos/autoload" nil t)
(+macos!open-with open-in-default-program)

;;;###autoload (autoload '+macos/reveal-in-finder "tools/macos/autoload" nil t)
(+macos!open-with reveal-in-finder "Finder" default-directory)

;;;###autoload (autoload '+macos/reveal-project-in-finder "tools/macos/autoload" nil t)
(+macos!open-with reveal-project-in-finder "Finder" (doom-project-root))

;;;###autoload (autoload '+macos/send-to-transmit "tools/macos/autoload" nil t)
(+macos!open-with send-to-transmit "Transmit")

;;;###autoload (autoload '+macos/send-cwd-to-transmit "tools/macos/autoload" nil t)
(+macos!open-with send-cwd-to-transmit "Transmit" default-directory)

;;;###autoload (autoload '+macos/send-to-launchbar "tools/macos/autoload" nil t)
(+macos!open-with send-to-launchbar "LaunchBar")

;;;###autoload (autoload '+macos/send-project-to-launchbar "tools/macos/autoload" nil t)
(+macos!open-with send-project-to-launchbar "LaunchBar" (doom-project-root))
