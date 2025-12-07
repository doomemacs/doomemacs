;;; os/macos/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +macos-defaults (action &rest args)
  (apply #'doom-call-process "defaults" action args))

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
         (args (cons "open"
                     (append (if app-name (list "-a" app-name))
                             (list path)))))
    (message "Running: %S" args)
    (apply #'doom-call-process args)))

(defmacro +macos--open-with (id &optional app dir)
  `(defun ,(intern (format "+macos/%s" id)) ()
     (interactive)
     (+macos-open-with ,app ,dir)))

(defmacro +macos--open-with-iterm (id &optional dir newwindow?)
  `(defun ,(intern (format "+macos/%s" id)) ()
     (interactive)
     (letf! ((defun read-newwindows ()
               (cdr (+macos-defaults
                     "read" "com.googlecode.iterm2" "OpenFileInNewWindows")))
             (defun write-newwindows (bool)
               (+macos-defaults
                "write" "com.googlecode.iterm2" "OpenFileInNewWindows"
                "-bool" (if bool "true" "false"))))
       (let ((newwindow?
              (if ,newwindow? (not (equal (read-newwindows) "1")))))
         (when newwindow?
           (write-newwindows t))
         (unwind-protect (+macos-open-with "iTerm" ,dir)
           (when newwindow?
             (write-newwindows nil)))))))

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
(+macos--open-with-iterm open-in-iterm default-directory)

;;;###autoload (autoload '+macos/open-in-iterm-new-window "os/macos/autoload" nil t)
(+macos--open-with-iterm open-in-iterm-new-window default-directory t)
